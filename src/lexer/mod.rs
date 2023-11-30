//! This module contains the lexer.
//!
//! TODO:
//!
//! - [ ] Make it possible to be an on demand lexer.
//! - [ ] Implement nested comments.
//! - [ ] Implement recovery.

mod cursor;
mod error;
mod span;
pub mod token;

use token::{
    cook_tokens, CommentKind, KeywordKind as KK, LiteralKind as LK, NumberBase,
    PunctuatorKind as PK, SpecialKeywordKind as SKK, Token, TokenKind as TK, TokenStream,
    Whitespace,
};

mod tests;

use error::Error;

type LResult<T> = Result<T, Error>;

fn is_alpha(c: &char) -> bool {
    let minor_case = c.is_ascii_lowercase();
    let major_case = c.is_ascii_uppercase();
    let underscore = c == &'_';
    minor_case || major_case || underscore
}
fn is_alpha_numeric(c: &char) -> bool {
    c.is_ascii_digit() || is_alpha(c)
}

fn is_lit_bool(s: &str) -> bool {
    ["true", "false"].contains(&s)
}

pub fn lex_tokens_from_file(source: &str) -> LResult<TokenStream> {
    let tokens = Lexer::from(source).scan_tokens();
    Ok(tokens)
}

struct Lexer {
    cursor: cursor::Cursor,

    errors: Vec<Error>,
}

fn maybe_add_to_token_cache(punc_cache: &mut Vec<Token>, token: Token, tokens: &mut Vec<Token>) {
    // In this part we combine simple punctuator tokens to complex ones, if possible

    // `punc_cache` is where copies of the previous punctuators lie,
    // which could be combined to a complex token.

    // If the scanned token is not a punctuator the `punc_cache` is flushed.

    // Generally speaking, there are special rules for combining structural punctuators into complex puncturators.
    //
    // `.`: Must be the first punctuator OR all the preceding punctuators must be `.`.
    // - valid examples: `.`, `.=`, `..`
    // - invalid examples: `=.`, `.=.`
    //
    // `:`: Must be the first punctuator OR all the preceding punctuators must be `:`
    // - valid examples: `:`, `::`
    // - invalid examples: `:=:`
    //
    // `=`: It can't form a complex token with a preceding `:`.

    // If it goes to `punc_cache` it is meant that it is cached to be potentially used for a complex token
    // To the `tokens` simply means it goes to the token stream.
    enum Action {
        ToTokens,
        ToPuncCache,
    }

    fn action_based_on_token_(punc_cache: &[Token], token: &Token) -> Action {
        if let TK::Punctuator(punc) = &token.kind {
            if punc.is_structural() {
                let (is_last_p_same, is_last_p_colonequal) = punc_cache
                    .last()
                    .map(|p| {
                        (
                            p.kind == TK::Punctuator(punc.clone()),
                            p.kind == TK::Punctuator(PK::Colon) && punc == &PK::Equal,
                        )
                    })
                    .unwrap_or((false, false));
                if [PK::Dot, PK::Colon].contains(punc)
                    && (punc_cache.is_empty() || is_last_p_same || is_last_p_colonequal)
                {
                    return Action::ToPuncCache;
                }
                return Action::ToTokens;
            } else {
                return Action::ToPuncCache;
            }
        }
        Action::ToTokens
    }

    fn action_based_on_token(punc_cache: &[Token], token: &Token) -> Action {
        let mut action = Action::ToTokens;
        if let TK::Punctuator(punc) = &token.kind {
            action = Action::ToPuncCache;
            if punc.is_structural() {
                action = Action::ToTokens;
                let (is_last_p_same, is_last_p_colonequal) = punc_cache
                    .last()
                    .map(|p| {
                        (
                            p.kind == TK::Punctuator(punc.clone()),
                            p.kind == TK::Punctuator(PK::Colon) && punc == &PK::Equal,
                        )
                    })
                    .unwrap_or((false, false));
                if [PK::Dot, PK::Colon].contains(punc) && (punc_cache.is_empty() || is_last_p_same)
                {
                    action = Action::ToPuncCache;
                    if is_last_p_colonequal {
                        action = Action::ToTokens;
                    }
                }
            }
        }
        action
    }

    let action = action_based_on_token(punc_cache, &token);
    match action {
        Action::ToTokens => {
            if !punc_cache.is_empty() {
                tokens.push(cook_tokens(punc_cache));
                punc_cache.clear();
            }
            if !token.kind.is_to_skip() {
                tokens.push(token);
            }
        }
        Action::ToPuncCache => punc_cache.push(token),
    }
}

fn handle_punc_cache(punc_cache: &Vec<Token>) -> Vec<Token> {
    if punc_cache.is_empty() {
        return Vec::new();
    }
    let punc_cache_kind = punc_cache
        .iter()
        .map(|x| {
            if let TK::Punctuator(punc) = &x.kind {
                punc
            } else {
                unreachable!()
            }
        })
        .collect::<Vec<_>>();

    let _: Vec<Token> = match punc_cache_kind.as_slice() {
        &[PK::Colon, PK::Equal] => {
            let pk_0 = punc_cache[0].clone();
            let pk_1 = punc_cache[1].clone();
            return vec![pk_0, pk_1];
        }
        &[PK::Equal, PK::Equal]
        | &[PK::Exclamation, PK::Equal]
        | &[PK::Dot, PK::Dot]
        | &[PK::Colon, PK::Colon] => {
            let pk_0 = punc_cache[0].clone();
            let pk_1 = punc_cache[1].clone();
            return vec![cook_tokens(&vec![pk_0, pk_1])];
        }
        // handles if any of the tokens is .is_punctuator() == true.
        a if a.iter().any(|x| x.is_structural()) => {
            let mut tokens = Vec::new();
            let mut non_struct_tokens = Vec::new();
            let mut i = 0;
            while i < punc_cache.len() {
                if !punc_cache_kind[i].is_structural() {
                    non_struct_tokens.push(punc_cache[i].clone());
                } else {
                    if !non_struct_tokens.is_empty() {
                        tokens.push(cook_tokens(&non_struct_tokens));
                        non_struct_tokens.clear();
                    }
                    tokens.push(punc_cache[i].clone());
                }
                i += 1;
            }
            return tokens;
        }
        _ => {
            return vec![cook_tokens(punc_cache)];
        }
    };
}
impl From<&str> for Lexer {
    fn from(source: &str) -> Self {
        Self {
            cursor: cursor::Cursor::new(source.chars().collect()),
            errors: Vec::new(),
        }
    }
}

impl Lexer {
    fn scan_tokens(&mut self) -> TokenStream {
        // NOTE: `punc_cache` has the magic number 5 as its capacity, because the assumption is that complex tokens will rarely be more than 5 symbols long.
        let mut punc_cache: Vec<Token> = Vec::with_capacity(5);
        let mut tokens: Vec<Token> = Vec::new();
        loop {
            match self.scan_token() {
                Ok(token) => {
                    if token.kind.is_punctuator() {
                        punc_cache.push(token);
                    } else {
                        // If the `punc_cache` is not empty and the token is not a punctuator,
                        // it means that it just switched, thus it is time to interpret the `punc_cache`.
                        if !punc_cache.is_empty() {
                            let handled_tokens = handle_punc_cache(&punc_cache);
                            for handled_token in handled_tokens {
                                tokens.push(handled_token);
                            }
                            punc_cache.clear();
                        }
                        if !token.kind.is_to_skip() {
                            tokens.push(token.clone());
                        }
                        if token.kind == TK::SpecialKeyword(SKK::Eof) {
                            break;
                        }
                    }
                }
                Err(e) => self.errors.push(e),
            }
        }
        TokenStream::from(tokens)
    }

    fn scan_token_kind(&mut self) -> LResult<(TK, span::Position, usize)> {
        // The assumption here is that [`None`] signifies the end of the file.
        // Also, assuming that `'\0'` is not a valid character in the source code.
        let c = *self.cursor.peek(0).unwrap_or(&'\0');

        let kind = match c {
            '/' => match self.cursor.peek(1) {
                Some('/') => self.lex_comment_line()?,
                Some('*') => self.lex_comment_block()?,
                _ => TK::Punctuator(PK::Slash),
            },
            '"' => self.lex_string()?,
            '\'' => self.lex_char()?,
            '\0' => {
                self.cursor.advance();
                TK::SpecialKeyword(SKK::Eof)
            }
            // TODO: Once if let guards are available rewrite it as such
            kw_punct if PK::try_from(kw_punct).is_ok() => {
                self.cursor.advance();
                TK::Punctuator(PK::try_from(kw_punct).expect("guaranteed"))
            }
            kw_special if SKK::try_from(kw_special).is_ok() => {
                self.cursor.advance();
                TK::SpecialKeyword(SKK::try_from(kw_special).expect("guaranteed"))
            }
            digit if digit.is_ascii_digit() => self.lex_number(),
            alpha if is_alpha(&alpha) => self.lex_identifier()?,
            unknown => return Err(Error::unknown_character(unknown, self.cursor.cursor)),
        };

        let mut end_pos = self.cursor.cursor;
        end_pos.column = self.cursor.cursor.column - 1;
        let end_index = self.cursor.index - 1;

        Ok((kind, end_pos, end_index))
    }

    /// Scans a token.
    ///
    /// This is one of the core functions of the lexer. Maybe in the future, in case the lexer becomes on demand,
    /// this will become the main function.
    ///
    /// Starts to scan from [`Lexer::index`] until it has a valid token [`Token`]. If the token is invalid, it returns an error [`Option::Some(Error)`] [`Error`].
    /// If the token is valid, it returns the token and the [`Lexer::index`] is set one position after the token, so that a new token can be scanned.
    // TODO: Add recovery.
    // TODO: The cursor/index should only here be moved to the position after the token. Make sure that it happens.
    fn scan_token(&mut self) -> LResult<Token> {
        let start_pos = self.cursor.cursor;
        let start_index = self.cursor.index;

        let (token_kind, end_pos, end_index) = self.scan_token_kind()?;

        if token_kind == TK::SpecialKeyword(SKK::Newline) {
            self.cursor.cursor.line += 1;
            self.cursor.cursor.column = 1;
        }

        let token = Token::new(
            token_kind,
            span::Span::new(start_pos, end_pos),
            Whitespace::from((
                *self.cursor.chars.get(start_index).unwrap_or(&' '),
                *self.cursor.chars.get(end_index).unwrap_or(&' '),
            )),
        );

        Ok(token)
    }

    fn lex_escape_char(&mut self) -> LResult<char> {
        self.cursor
            .eat_char('\\')
            .map_err(|x| Error::expected_char('\\', x.copied()))?;

        let escaped_char = match self.cursor.peek(0) {
            Some('n') => '\n',
            Some('r') => '\r',
            Some('t') => '\t',
            Some('\\') => '\\',
            Some('\'') => '\'',
            Some('"') => '"',
            Some('0') => '\0',
            Some(c) => return Err(Error::unknown_escape_char(*c)),
            None => return Err(Error::unterminated_char_lit()),
        };
        self.cursor.advance();
        Ok(escaped_char)
    }

    /// Lexes a character literal.
    ///
    /// # Panics
    /// If the first character is not `\'`.
    fn lex_char(&mut self) -> LResult<TK> {
        self.cursor
            .eat_char('\'')
            .map_err(|x| Error::expected_char('\'', x.copied()))?;
        // '\''
        let mut escaped_char = false;
        let c = match self.cursor.peek(0) {
            None => return Err(Error::unterminated_char_lit()),
            Some('\'') => return Err(Error::empty_char_literal()),
            Some('\n') | Some('\r') => return Err(Error::new_line_in_char_lit()),
            Some('\\') => {
                let x = self.lex_escape_char()?;
                escaped_char = true;
                x
            }
            Some(c) => {
                let cc = *c;
                self.cursor.advance();
                cc
            }
        };

        match self.cursor.eat_char('\'') {
            Ok(..) => {}
            Err(..) => {
                // TODO: This is not correct. Fix this!
                if escaped_char {
                    return Err(Error::unterminated_char_lit());
                } else {
                    return Err(Error::char_lit_contains_multiple_codepoints());
                }
            }
        };
        Ok(TK::Literal(LK::Char(c)))
    }

    fn lex_string(&mut self) -> LResult<TK> {
        self.cursor
            .eat_char('\"')
            .map_err(|x| Error::expected_char('\"', x.copied()))?;

        let mut string_content = String::new();
        while let Some(c) = self.cursor.peek(0) {
            if c == &'"' {
                break;
            }

            match c {
                '\\' => {
                    let escape_char = self.lex_escape_char()?;
                    string_content.push(escape_char);
                }
                '\n' => {
                    self.cursor.advance();
                    self.cursor.cursor.line += 1;
                    self.cursor.cursor.column = 1;
                }
                '\t' => {
                    // TODO: Figure out how to correctly handle a tab, because they can be variable length.
                    const TAB_SIZE: u32 = 1;
                    self.cursor.advance();
                    self.cursor.cursor.column += TAB_SIZE;
                }
                '\r' => {
                    self.cursor.advance();
                    self.cursor.cursor.column = 1;
                }
                _ => {
                    string_content.push(*c);
                    self.cursor.advance();
                }
            }
        }
        self.cursor
            .eat_char('\"')
            .map_err(|x| Error::expected_char('\"', x.copied()))?;

        if self.cursor.peek(0).is_none() {
            return Err(Error::unterminated_string());
        }
        Ok(TK::Literal(LK::Str(string_content)))
    }

    /// Lexes a line comment.
    ///
    /// # Panics
    /// If it doesn't start with `//`.
    fn lex_comment_line(&mut self) -> LResult<TK> {
        self.cursor.eat_str("//").expect("this was checked before");

        let mut comment_content = String::new();
        while let Some(c) = self.cursor.peek(0) {
            if c == &'\n' || c == &'\r' || c == &'\0' {
                break;
            }
            comment_content.push(*c);
            self.cursor.advance();
        }
        Ok(TK::SpecialKeyword(SKK::Comment(CommentKind::Line(
            comment_content,
        ))))
    }

    /// Lexes a block comment.
    ///
    /// # Panics
    /// If it doesn't start with `/*`.
    fn lex_comment_block(&mut self) -> LResult<TK> {
        // TODO: Implement nested block comments
        self.cursor.eat_str("/*").expect("this was checked before");

        let mut comment_content = String::new();
        while let Some(c) = self.cursor.peek(0) {
            if c == &'*' && self.cursor.check_char(1, '/').is_ok() {
                self.cursor.eat_str("*/").expect("this was checked before");
                break;
            }
            if c == &'\0' {
                return Err(Error::unterminated_block_comment());
            }

            if c == &'\r' && self.cursor.check_char(1, '\n').is_ok() {
                self.cursor.advance();
                self.cursor.advance();
                self.cursor.cursor.line += 1;
                self.cursor.cursor.column = 1;
                comment_content.push('\n');
            } else {
                let cc = *c;
                self.cursor.advance();
                comment_content.push(cc);
            }
        }
        Ok(TK::SpecialKeyword(SKK::Comment(CommentKind::Block(
            comment_content,
        ))))
    }

    fn lex_number(&mut self) -> TK {
        assert!(
            self.cursor.peek(0).unwrap().is_ascii_digit(),
            "this was checked before"
        );
        /// Processes the prefix of a number.
        ///
        /// The valid prefixes are: `0b`, `0o`, `0d`, `0x`.
        /// These prefixes are hardcoded, thus `0q` in `0q01` can never be a valid prefix.
        /// However, suffixes are not hardcoded (at least they are not planned to),
        /// thus the previous number, `0q01`, could actually potentially be the number literal `0` with the suffix `q01`.
        /// Because of that, if no prefix is recognized, we simply skip the prefix processing, to give a change to parse it as a suffix.
        fn process_prefix(this: &mut Lexer) -> Option<NumberBase> {
            match (this.cursor.peek(0), this.cursor.peek(1)) {
                (Some('0'), Some(c)) if c.is_alphabetic() => {
                    let prefix = format!("0{}", c);
                    NumberBase::try_from(prefix.as_str())
                        .map(|x| {
                            this.cursor.advance();
                            this.cursor.advance();
                            x
                        })
                        .ok()
                }
                _ => None,
            }
        }

        fn process_number_body(this: &mut Lexer, allow_letters: bool) -> (String, bool, bool) {
            let mut is_after_dot = false;
            let mut is_floating = false;
            let mut has_suffix = false;
            let mut string = String::new();

            while let Some(peeked) = this.cursor.peek(0) {
                let res: LResult<()> = match peeked {
                    // checks whether it is a possible digit at all.
                    c if (allow_letters && c.is_ascii_hexdigit())
                        || (!allow_letters && c.is_ascii_digit()) =>
                    {
                        string.push(*c);
                        if is_after_dot {
                            is_after_dot = false;
                        }
                        Ok(())
                    }
                    // this parses a potential suffix
                    c if is_alpha(c) => {
                        has_suffix = true;
                        break;
                    }
                    // in a number there can be only one dot
                    '.' if !is_floating && this.cursor.check_char(1, '.').ok().is_none() => {
                        string.push(*peeked);
                        is_floating = true;
                        is_after_dot = true;
                        Ok(())
                    }
                    // a '_' is not possible after a '.'. E.g. '1._3' is not a number, this is an integer followed by an identifier.
                    '_' if !is_after_dot => Ok(()),
                    _ => break,
                };
                // if let Err(e) = res {
                //     return Err(e);
                // }
                res.unwrap();
                this.cursor.advance();
            }
            (string, is_floating, has_suffix)
        }

        fn process_suffix(this: &mut Lexer, mode_parse_suffix: bool) -> Option<String> {
            if mode_parse_suffix {
                let mut suffix: String = String::new();
                while this.cursor.peek(0).unwrap().is_alphanumeric() {
                    suffix.push(*this.cursor.peek(0).unwrap());
                    this.cursor.advance();
                }
                Some(suffix)
            } else {
                None
            }
        }
        fn check_for_errors(
            string: String,
            is_floating: bool,
            suffix: Option<String>,
            prefix: Option<NumberBase>,
        ) -> Vec<token::NumberError> {
            let mut errors = Vec::new();

            if string.is_empty() && prefix.is_some() {
                errors.push(token::NumberError::PrefixWithoutNumber);
            }

            // Ox10.10 is not valid, because floating point numbers can't have a prefix or rather have to be decimal.
            // NOTE: Maybe, the prefix `0d` will be allowed, because it is the decimal prefix.
            if prefix.is_some() && is_floating {
                errors.push(token::NumberError::FloatWithPrefix);
            }

            // Ob45 is not valid, because binary numbers can only have 0 and 1 as digits.
            if !string.chars().all(|c| {
                prefix.unwrap_or(NumberBase::Decimal).is_char_in(c) || c == '.' || c == '_'
            }) {
                errors.push(token::NumberError::NumberNotInBase);
            }
            if let Some(suffix) = &suffix {
                // TODO: Move this part to the semantic analysis. These hardcoded suffixes are only temporary
                //     The goal is to have al suffixes be custom.
                let possible_suffixes = [
                    "i8", "i16", "i32", "i64", // signed integers
                    "u8", "u16", "u32", "u64", // unsigned integers
                    "f32", "f64", // floating point numbers
                ];
                if !possible_suffixes.contains(&suffix.clone().as_str()) {
                    errors.push(token::NumberError::InvalidSuffix);
                }
            }
            errors
        }

        let number_base_prefix = process_prefix(self);
        let (string, is_floating, has_suffix) =
            process_number_body(self, number_base_prefix.is_some());
        let suffix = process_suffix(self, has_suffix);

        // Error handling.
        // Right now it can only handle one error, but in the future it should be able to handle multiple errors.
        let errors = check_for_errors(
            string.clone(),
            is_floating,
            suffix.clone(),
            number_base_prefix,
        );
        assert!(errors.len() < 2, "there can be only one error at a time");
        let lit = if errors.len() == 1 {
            let number_with_error = token::Number::new(
                string.clone(),
                number_base_prefix,
                suffix.clone(),
                is_floating,
                Some(errors[0].clone()),
            );
            LK::Number(number_with_error)
        } else if is_floating {
            LK::floating(&string, suffix)
        } else {
            LK::integer(&string, number_base_prefix, suffix)
        };
        TK::Literal(lit)
    }

    fn lex_identifier(&mut self) -> LResult<TK> {
        let content: String = self
            .cursor
            .eat_while(is_alpha_numeric)
            .into_iter()
            .collect();

        let token = match content.as_ref() {
            lit if is_lit_bool(lit) => LK::try_from(lit).map_or_else(TK::Identifier, TK::Literal),
            kw => KK::try_from(kw).map_or_else(TK::Identifier, TK::Keyword),
        };
        Ok(token)
    }
}
