//! This module contains the lexer.
//!
//! TODO:
//!
//! - [ ] Maybe in the future, this module should be elevated into one's own crate.
//! - [ ] Make it possible to be an on demand lexer.
//! - [ ] Implement nested comments.
//! - [ ] Consider adding different comment styles.

mod cursor;
mod error;
use token::{
    cook_tokens, CommentKind, KeywordKind, LiteralKind, NumberBase, PunctuatorKind, Span,
    SpecialKeywordKind, Token, TokenKind, Whitespace,
};
pub mod token;

mod tests;

use error::Error;

type LResult<T> = Result<T, Error>;

fn is_alpha(c: &char) -> bool {
    let minor_case = ('a'..='z').contains(c);
    let major_case = ('A'..='Z').contains(c);
    let underscore = c == &'_';
    minor_case || major_case || underscore
}
fn is_alpha_numeric(c: &char) -> bool {
    c.is_ascii_digit() || is_alpha(c)
}

fn is_lit_bool(s: &str) -> bool {
    ["true", "false"].contains(&s)
}

pub fn lex_tokens_from_file(source: &str) -> LResult<Vec<Token>> {
    let tokens = Lexer::from(source).scan_tokens();
    Ok(tokens)
}

struct Lexer {
    chars: Vec<char>,

    index: usize,
    cursor: token::Position,
}

fn maybe_add_to_token_cache(punc_cache: &mut Vec<Token>, token: Token, tokens: &mut Vec<Token>) {
    use PunctuatorKind::*;
    use TokenKind::*;
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
        if let Punctuator(punc) = &token.kind {
            if punc.is_structural() {
                let (is_last_p_same, is_last_p_colonequal) = punc_cache
                    .last()
                    .map(|p| {
                        (
                            p.kind == Punctuator(punc.clone()),
                            p.kind == Punctuator(Colon) && punc == &Equal,
                        )
                    })
                    .unwrap_or((false, false));
                if [Dot, Colon].contains(punc)
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
        if let Punctuator(punc) = &token.kind {
            action = Action::ToPuncCache;
            if punc.is_structural() {
                action = Action::ToTokens;
                let (is_last_p_same, is_last_p_colonequal) = punc_cache
                    .last()
                    .map(|p| {
                        (
                            p.kind == Punctuator(punc.clone()),
                            p.kind == Punctuator(Colon) && punc == &Equal,
                        )
                    })
                    .unwrap_or((false, false));
                if [Dot, Colon].contains(punc) && (punc_cache.is_empty() || is_last_p_same) {
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

impl From<&str> for Lexer {
    fn from(source: &str) -> Self {
        Self {
            chars: source.chars().collect(),
            index: 0,
            cursor: token::Position::new(1, 1),
        }
    }
}

impl Lexer {
    fn scan_tokens(&mut self) -> Vec<Token> {
        use TokenKind::*;

        // NOTE: `punc_cache` has the magic number 5 as its capacity, because the assumption is that complex tokens will rarely be more than 5 symbols long.
        let mut punc_cache: Vec<Token> = Vec::with_capacity(5);
        let mut tokens = Vec::new();
        while let Ok(token) = self.scan_token() {
            if token.kind == SpecialKeyword(SpecialKeywordKind::Eof) {
                if !punc_cache.is_empty() {
                    tokens.push(cook_tokens(&punc_cache));
                    punc_cache.clear();
                }
                tokens.push(token);
                break;
            }
            maybe_add_to_token_cache(&mut punc_cache, token, &mut tokens);
        }
        tokens
    }

    fn scan_token_kind(&mut self) -> LResult<(TokenKind, token::Position, usize)> {
        use PunctuatorKind::*;
        use SpecialKeywordKind::*;
        use TokenKind::*;

        // The assumption here is that `None` signifies the end of the file.
        // Also, assuming that `'\0'` is not a valid character in the source code.
        let c = self.peek(0).unwrap_or('\0');

        let kind = match c {
            '/' => match self.peek(1) {
                Some('/') => self.lex_comment_line()?,
                Some('*') => self.lex_comment_block()?,
                _ => Punctuator(Slash),
            },
            '"' => self.lex_string()?,
            '\'' => self.lex_char()?,
            '\0' => {
                self.advance();
                SpecialKeyword(Eof)
            }
            // TODO: Once if let guards are available rewrite it as such
            kw_punct if PunctuatorKind::try_from(kw_punct).is_ok() => {
                self.advance();
                Punctuator(PunctuatorKind::try_from(kw_punct).unwrap())
            }
            kw_special if SpecialKeywordKind::try_from(kw_special).is_ok() => {
                self.advance();
                SpecialKeyword(SpecialKeywordKind::try_from(kw_special).unwrap())
            }
            digit if digit.is_ascii_digit() => self.lex_number()?,
            alpha if is_alpha(&alpha) => self.lex_identifier()?,
            unknown => return Err(Error::unknown_character(unknown, self.cursor)),
        };

        let mut end_pos = self.cursor;
        end_pos.column = self.cursor.column - 1;
        let end_index = self.index - 1;

        Ok((kind, end_pos, end_index))
    }

    /// Scans a token.
    ///
    /// This is one of the core functions of the lexer. Maybe in the future, in case the lexer becomes on demand,
    /// this will become the main function.
    ///
    /// Starts to scan from [`Lexer::index`] until it has a valid token [`Token`]. If the token is invalid, it returns an error [`Error`].
    /// If the token is valid, it returns the token and the [`Lexer::index`] is set one position after the token, so that a new token can be scanned.
    // TODO: Add recovery.
    // TODO: The cursor/index should only here be moved to the position after the token. Make sure that it happens.
    fn scan_token(&mut self) -> LResult<Token> {
        let start_pos = self.cursor;
        let start_index = self.index;

        let (token_kind, end_pos, end_index) = self.scan_token_kind()?;

        if token_kind == TokenKind::SpecialKeyword(SpecialKeywordKind::Newline) {
            self.cursor.line += 1;
            self.cursor.column = 1;
        }

        let token = Token::new(
            token_kind,
            Span::new(start_pos, end_pos),
            Whitespace::from((
                *self.chars.get(start_index).unwrap_or(&' '),
                *self.chars.get(end_index).unwrap_or(&' '),
            )),
        );

        Ok(token)
    }

    fn lex_escape_char(&mut self) -> LResult<char> {
        self.eat_char('\\')
            .map_err(|x| Error::expected_char('\\', x))?;

        let escaped_char = match self.peek(0) {
            Some('n') => '\n',
            Some('r') => '\r',
            Some('t') => '\t',
            Some('\\') => '\\',
            Some('\'') => '\'',
            Some('"') => '"',
            Some('0') => '\0',
            Some(c) => return Err(Error::unknown_escape_char(c)),
            None => return Err(Error::unterminated_char_lit()),
        };
        self.advance();
        Ok(escaped_char)
    }

    /// Lexes a character literal.
    ///
    /// # Panics
    /// If the first character is not `\'`.
    fn lex_char(&mut self) -> LResult<TokenKind> {
        use LiteralKind::*;
        use TokenKind::*;
        self.eat_char('\'')
            .map_err(|x| Error::expected_char('\'', x))?;
        // '\''
        let mut escaped_char = false;
        let c = match self.peek(0) {
            None => return Err(Error::unterminated_char_lit()),
            Some('\'') => return Err(Error::empty_char_literal()),
            Some('\n') | Some('\r') => return Err(Error::new_line_in_char_lit()),
            Some('\\') => {
                let x = self.lex_escape_char()?;
                escaped_char = true;
                x
            }
            Some(c) => {
                self.advance();
                c
            }
        };

        match self.eat_char('\'') {
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
        Ok(Literal(Char(c)))
    }
    fn lex_string(&mut self) -> LResult<TokenKind> {
        use LiteralKind::*;
        use TokenKind::*;
        self.eat_char('\"')
            .map_err(|x| Error::expected_char('\"', x))?;

        let mut string_content = String::new();
        while let Some(c) = self.peek(0) {
            if c == '"' {
                break;
            }

            match c {
                '\\' => {
                    let escape_char = self.lex_escape_char().unwrap();
                    string_content.push(escape_char);
                }
                '\n' => {
                    self.advance();
                    self.cursor.line += 1;
                    self.cursor.column = 1;
                }
                '\t' => {
                    // TODO: Figure out how to correctly handle a tab, because they can be variable length.
                    const TAB_SIZE: u32 = 1;
                    self.advance();
                    self.cursor.column += TAB_SIZE;
                }
                '\r' => {
                    self.advance();
                    self.cursor.column = 1;
                }
                _ => {
                    string_content.push(c);
                    self.advance();
                }
            }
        }
        self.eat_char('\"')
            .map_err(|x| Error::expected_char('\"', x))?;

        if self.peek(0).is_none() {
            return Err(Error::unterminated_string());
        }
        Ok(Literal(Str(string_content)))
    }

    /// Lexes a line comment.
    ///
    /// # Panics
    /// If it doesn't start with `//`.
    fn lex_comment_line(&mut self) -> LResult<TokenKind> {
        use SpecialKeywordKind::*;
        use TokenKind::*;
        self.eat_str("//").unwrap();

        let mut comment_content = String::new();
        while let Some(c) = self.peek(0) {
            if c == '\n' || c == '\r' || c == '\0' {
                break;
            }
            comment_content.push(c);
            self.advance();
        }
        Ok(SpecialKeyword(Comment(CommentKind::Line(comment_content))))
    }
    /// Lexes a block comment.
    ///
    /// # Panics
    /// If it doesn't start with `/*`.
    fn lex_comment_block(&mut self) -> LResult<TokenKind> {
        use SpecialKeywordKind::*;
        use TokenKind::*;
        // TODO: Implement nested block comments
        self.eat_str("/*").unwrap();

        let mut comment_content = String::new();
        while let Some(c) = self.peek(0) {
            if c == '*' && self.check_char(1, '/').is_ok() {
                self.eat_str("*/").expect("this was checked before");
                break;
            }
            if c == '\0' {
                return Err(Error::unterminated_block_comment());
            }

            if c == '\r' && self.check_char(1, '\n').is_ok() {
                self.advance();
                self.advance();
                self.cursor.line += 1;
                self.cursor.column = 1;
                comment_content.push('\n');
            } else {
                self.advance();
                comment_content.push(c);
            }
        }
        Ok(SpecialKeyword(Comment(CommentKind::Block(comment_content))))
    }

    fn lex_number(&mut self) -> LResult<TokenKind> {
        use TokenKind::*;

        fn process_prefix(this: &mut Lexer) -> Result<Option<NumberBase>, Error> {
            match (this.peek(0), this.peek(1)) {
                (Some('0'), Some(c)) if c.is_alphabetic() => {
                    let pot_prefix = "0".to_owned() + &c.to_string();
                    NumberBase::try_from(pot_prefix.as_str())
                        .map(|x| {
                            this.advance();
                            this.advance();
                            Some(x)
                        })
                        .map_err(|_| Error::not_recognized_base_prefix(&pot_prefix))
                }
                _ => Ok(None),
            }
        }

        fn process_number_body(
            this: &mut Lexer,
            allow_letters: bool,
        ) -> Result<(String, bool, bool), Error> {
            let mut is_after_dot = false;
            let mut is_floating = false;
            let mut mode_parse_suffix = false;
            let mut string = String::new();

            while let Some(peeked) = this.peek(0) {
                let res: LResult<()> = match peeked {
                    // checks whether it is a possible digit at all.
                    c if (allow_letters && c.is_ascii_hexdigit())
                        || (!allow_letters && c.is_ascii_digit()) =>
                    {
                        string.push(c);
                        if is_after_dot {
                            is_after_dot = false;
                        }
                        Ok(())
                    }
                    // this parses a potential suffix
                    c if is_alpha(&c) => {
                        mode_parse_suffix = true;
                        break;
                    }
                    // in a number there can be only one dot
                    '.' if !is_floating && this.check_char(1, '.').ok().is_none() => {
                        string.push(peeked);
                        is_floating = true;
                        is_after_dot = true;
                        Ok(())
                    }
                    // a '_' is not possible after a '.'. E.g. '1._3' is not a number, this is an integer followed by an identifier.
                    '_' if !is_after_dot => Ok(()),
                    _ => break,
                };
                if let Err(e) = res {
                    return Err(e);
                }
                this.advance();
            }
            Ok((string, is_floating, mode_parse_suffix))
        }

        fn process_suffix(this: &mut Lexer, mode_parse_suffix: bool) -> Option<String> {
            if mode_parse_suffix {
                let mut suffix: String = String::new();
                while this.peek(0).unwrap().is_alphanumeric() {
                    suffix.push(this.peek(0).unwrap());
                    this.advance();
                }
                Some(suffix)
            } else {
                None
            }
        }

        let number_base_prefix = process_prefix(self)?;
        let (string, is_floating, mode_parse_suffix) =
            process_number_body(self, number_base_prefix.is_some())?;
        let suffix = process_suffix(self, mode_parse_suffix);

        // Error handling
        if number_base_prefix.is_some() && is_floating {
            return Err(Error::floats_dont_have_base_prefix(number_base_prefix));
        }
        if !string.chars().all(|c| {
            number_base_prefix
                .unwrap_or(NumberBase::Decimal)
                .is_char_in(c)
        }) {
            return Err(Error::invalid_digit_base_prefix(number_base_prefix));
        }

        let possible_suffixes = [
            "", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64",
        ];
        if !possible_suffixes.contains(&suffix.clone().unwrap_or_default().as_str()) {
            return Err(Error::invalid_digit_type_suffix(suffix));
        }
        assert_ne!(string.len(), 0);

        let lit = Literal(if is_floating {
            LiteralKind::floating(&string, suffix)
        } else {
            LiteralKind::integer(&string, number_base_prefix, suffix)
        });

        Ok(lit)
    }

    fn lex_identifier(&mut self) -> LResult<TokenKind> {
        use TokenKind::*;
        let content = self.eat_while(is_alpha_numeric);
        let token = match content.as_ref() {
            lit if is_lit_bool(lit) => LiteralKind::try_from(lit).map_or_else(Identifier, Literal),
            kw => KeywordKind::try_from(kw).map_or_else(Identifier, Keyword),
        };
        Ok(token)
    }
}
