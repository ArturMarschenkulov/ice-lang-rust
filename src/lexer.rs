//! This module contains the lexer.
//!
//! TODO:
//!
//! - [ ] Maybe in the future, this module should be elevated into one's own crate.
//! - [ ] Make it possible to be an on demand lexer.
//! - [ ] Implement nested comments.
//! - [ ] Consider adding different comment styles.

use crate::error::LexerError;
use crate::token;
use crate::token::{
    cook_tokens, CommentKind, KeywordKind, LiteralKind, NumberBase, PunctuatorKind,
    SpecialKeywordKind, Token, TokenKind,
};

type LResult<T> = Result<T, LexerError>;

pub fn is_digit(c: &char) -> bool {
    ('0'..='9').contains(c)
}
pub fn is_hexadecimal(c: &char) -> bool {
    // let is_hd = c.is_digit(16);
    // let is_lower_case = c.is_uppercase();
    // is_hd && is_lower_case

    [('0'..='9'), ('a'..='f'), ('A'..='F')]
        .iter()
        .any(|s| s.contains(c))

    // let num = ('0'..='9').contains(c);
    // let upper = ('A'..='F').contains(c);
    // let lower = ('a'..='f').contains(c);
    // num || upper || lower
}
pub fn is_octal(c: &char) -> bool {
    // c.is_digit(8)
    ('0'..='7').contains(c)
}
pub fn is_binary(c: &char) -> bool {
    // c.is_digit(2)
    ('0'..='1').contains(c)
}
fn is_alpha(c: &char) -> bool {
    // let is_alphabetic = c.is_alphabetic();
    // let underscore = c == &'_';
    // is_alphabetic || underscore

    let minor_case = ('a'..='z').contains(c);
    let major_case = ('A'..='Z').contains(c);
    let underscore = c == &'_';
    minor_case || major_case || underscore
}
fn is_alpha_numeric(c: &char) -> bool {
    // c.is_alphanumeric()
    is_digit(c) || is_alpha(c)
}
fn is_left_whitespace(c: &char) -> bool {
    c == &' '
}
fn is_right_whitespace(c: &char) -> bool {
    c == &' '
}

fn is_lit_bool(s: &str) -> bool {
    ["true", "false"].contains(&s)
}

pub fn lex_tokens_from_file(source: &str) -> LResult<Vec<Token>> {
    let tokens = Lexer::new_from_str(source).scan_tokens();
    Ok(tokens)
}

/// A cursor for iterating over characters.
///
// TODO: Right now, this type is not used, but it should be integrated in the future,
//     for better separation of concerns.
struct CharCursor<'a> {
    chars: &'a Vec<char>,
    index: usize,
    cursor: token::Position,
}

struct Lexer {
    chars: Vec<char>,

    index: usize,
    cursor: token::Position,
}

impl Lexer {
    fn new_from_str(source: &str) -> Self {
        Self {
            chars: source.chars().collect(),
            index: 0,
            cursor: token::Position::new(1, 1),
        }
    }
    fn scan_tokens(&mut self) -> Vec<Token> {
        use PunctuatorKind::*;
        use SpecialKeywordKind::*;
        use TokenKind::*;

        // NOTE: `punc_cache` has the magic number 5 as its capacity, because the assumption is that complex tokens will rarely be more than 5 symbols long.
        let mut punc_cache: Vec<Token> = Vec::with_capacity(5);
        let mut tokens = Vec::new();
        while let Some(ch) = self.peek(0) {
            let token = self.scan_token(ch).unwrap();

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
            let mut action = Action::ToTokens;

            let punc_last_kind = punc_cache.last().map(|p| p.kind.clone());

            if let Punctuator(punc) = &token.kind {
                action = Action::ToPuncCache;

                if punc.is_structural() {
                    action = Action::ToTokens;
                    let (is_last_p_same, is_last_p_colonequal) = punc_last_kind
                        .as_ref()
                        .map(|p| {
                            (
                                p == &Punctuator(punc.clone()),
                                p == &Punctuator(Colon) && punc == &Equal,
                            )
                        })
                        .unwrap_or((false, false));
                    if [Dot, Colon].contains(punc) && (punc_cache.is_empty() || is_last_p_same) {
                        action = Action::ToPuncCache;
                        if is_last_p_colonequal {
                            action = Action::ToPuncCache;
                        }
                    }
                }
            }

            match action {
                Action::ToTokens => {
                    if !punc_cache.is_empty() {
                        tokens.push(cook_tokens(&punc_cache));
                        punc_cache.clear();
                    }
                    if !token.kind.is_to_skip() {
                        tokens.push(token);
                    }
                }
                Action::ToPuncCache => punc_cache.push(token),
            }
        }
        if !punc_cache.is_empty() {
            tokens.push(cook_tokens(&punc_cache));
            punc_cache.clear();
        }

        let token_eof = Token::eof();
        tokens.push(token_eof);
        tokens
    }

    fn scan_token_kind(&mut self, c: char) -> LResult<(TokenKind, token::Position, usize)> {
        use PunctuatorKind::*;
        use SpecialKeywordKind::*;
        use TokenKind::*;

        let index_old = self.index;
        let kind = match c {
            '/' => match self.peek_n_times_and_collect(1).unwrap().as_str() {
                "//" => self.lex_comment_line(),
                "/*" => self.lex_comment_block(),
                _ => Ok(Punctuator(Slash)),
            },
            '"' => self.lex_string(),
            '\'' => self.lex_char(),
            // TODO: Once if let guards are available rewrite it as such
            p if PunctuatorKind::from_char(p).is_some() => {
                Ok(Punctuator(PunctuatorKind::from_char(p).unwrap()))
            }
            p if SpecialKeywordKind::from_char(p).is_some() => {
                Ok(SpecialKeyword(SpecialKeywordKind::from_char(p).unwrap()))
            }
            cc if is_digit(&cc) => self.lex_number(),
            cc if is_alpha(&cc) => self.lex_identifier(),
            cc => Err(LexerError::unknown_character(cc, self.cursor)),
        };
        let kind = kind?;
        let index_new = self.index;
        // NOTE: If the token is simple we advance here, because the match statements look prettier that way.
        // With more complex tokens, basically everything except single punctuators,
        // handle the advance in their own functions
        // if kind.is_simple() {
        //     self.advance();
        // }
        if index_old == index_new {
            self.advance();
        }

        let end_pos = self.cursor;
        let end_index = self.index;

        if kind == SpecialKeyword(Newline) {
            self.cursor.line += 1;
            self.cursor.column = 1;
        } else {
            self.cursor.column += 1;
        }

        Ok((kind, end_pos, end_index))
    }

    /// Scans a token
    /// Returns the token which was lexed. It also moves the 'cursor' to the next character, so that one can lex the next token.
    fn scan_token(&mut self, c: char) -> LResult<Token> {
        let start_pos = self.cursor;
        let start_index = self.index;

        let (token_kind, end_pos, end_index) = self.scan_token_kind(c).unwrap();

        let whitespace = {
            let index_start = 0 - (end_index - start_index) as isize;
            let index_end = -1_isize;

            let index_start_left = self.peek(index_start - 1).unwrap_or(' ');
            let index_end_right = self.peek(index_end + 1).unwrap_or(' ');

            let ws_left = is_left_whitespace(&index_start_left);
            let ws_right = is_right_whitespace(&index_end_right);

            token::Whitespace::from((ws_left, ws_right))
        };

        let token = Token::new(token_kind, token::Span::new(start_pos, end_pos), whitespace);
        Ok(token)
    }
    fn lex_escape_char(&mut self) -> LResult<char> {
        self.eat_char('\\').unwrap();
        self.cursor.column += 1;

        let escaped_char = match self.peek(0) {
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('t') => Ok('\t'),
            Some('\\') => Ok('\\'),
            Some('\'') => Ok('\''),
            Some('"') => Ok('"'),
            Some('0') => Ok('\0'),
            Some(c) => Err(LexerError::unknown_escape_char(c)),
            None => Err(LexerError::unterminated_char_lit()),
        };
        self.advance();
        self.cursor.column += 1;

        self.cursor.column -= 1;

        escaped_char
    }
    fn lex_char(&mut self) -> LResult<TokenKind> {
        use LiteralKind::*;
        use TokenKind::*;
        self.eat_char('\'').unwrap();

        let c = match self.peek(0) {
            Some('\'') => return Err(LexerError::empty_char_literal()),
            Some('\n') | Some('\r') => return Err(LexerError::new_line_in_char_lit()),
            Some('\\') => self.lex_escape_char()?,
            Some(c) => {
                self.advance();
                c
            }
            None => return Err(LexerError::unterminated_char_lit()),
        };

        match self.eat_char('\'') {
            Ok(..) => {}
            Err(..) => return Err(LexerError::char_lit_contains_multiple_codepoints()),
        };
        Ok(Literal(Char(c)))
    }
    fn lex_string(&mut self) -> LResult<TokenKind> {
        use LiteralKind::*;
        use TokenKind::*;
        self.eat_char('\"').unwrap();
        self.cursor.column += 1;

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
                    self.advance();
                    self.cursor.column += 1;
                }
                '\r' => {
                    self.advance();
                    self.cursor.column = 1;
                }
                _ => {
                    string_content.push(c);
                    self.advance();
                    self.cursor.column += 1;
                }
            }
        }
        self.eat_char('\"').unwrap();
        self.cursor.column += 1;

        self.cursor.column -= 1;

        if self.peek(0).is_none() {
            return Err(LexerError::unterminated_string());
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
        // TODO: Implement that a comment at the end of a file is possible, meaning when it does not end through a newline, but eof
        self.eat_str("//").unwrap();
        self.cursor.column += 2;
        while self.peek(0) != Some('\n') && self.peek(0).is_some() {
            self.advance();
            self.cursor.column += 1;
        }
        self.cursor.column -= 1;
        //assert!(self.peek(0) == Some('\n'));
        Ok(SpecialKeyword(Comment(CommentKind::Line)))
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
        self.cursor.column += 2;

        while self.check_str("*/").is_empty() {
            if self.is_eof() {
                return Err(LexerError::unterminated_block_comment());
            }
            self.advance();

            if self.peek(0) == Some('\n') {
                self.cursor.line += 1;
                self.cursor.column = 1;
            } else {
                self.cursor.column += 1;
            }
        }

        self.eat_str("*/").expect("this was checked before");
        self.cursor.column += 2;

        self.cursor.column -= 1;

        Ok(SpecialKeyword(Comment(CommentKind::Block)))
    }

    fn lex_number(&mut self) -> LResult<TokenKind> {
        use LiteralKind::*;
        use TokenKind::*;

        fn process_prefix(lexer: &mut Lexer) -> Option<String> {
            let prefix = lexer.peek_n_times_and_collect(1)?;

            if prefix.starts_with('0') && prefix.chars().nth(1).map_or(false, |c| c.is_alphabetic())
            {
                lexer.advance();
                lexer.advance();
                lexer.cursor.column += 2;
                Some(prefix)
            } else {
                None
            }
        }
        fn process_prefix_(this: &mut Lexer) -> Option<String> {
            match this.peek_n_times_and_collect(1) {
                Some(suf) => {
                    let b_0 = (*suf.as_bytes().first().unwrap() as char) == '0';
                    let b_1 = (*suf.as_bytes().get(1).unwrap() as char).is_alphabetic();
                    if b_0 && b_1 {
                        this.advance();
                        this.advance();
                        this.cursor.column += 2;
                        Some(suf)
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        fn process_number_body(
            this: &mut Lexer,
            allow_letters: bool,
        ) -> Result<(String, bool, bool), LexerError> {
            let mut is_after_dot = false;
            let mut is_floating = false;
            let mut mode_parse_suffix = false;
            let mut string = String::new();

            while let Some(peeked) = this.peek(0) {
                let res: LResult<()> = match peeked {
                    // checks whether it is a possible digit at all.
                    c if (allow_letters && is_hexadecimal(&c))
                        || (!allow_letters && is_digit(&c)) =>
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
                    '.' if !is_floating && this.check_char('.', 1).ok().is_none() => {
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
                } else {
                    this.advance()
                }
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
        type TempComplexType = Result<(Option<NumberBase>, fn(&char) -> bool), LexerError>;
        fn determine_number_base(prefix: &Option<String>) -> TempComplexType {
            Ok(match prefix.as_deref() {
                Some("0b") => (Some(NumberBase::Binary), is_binary),
                Some("0o") => (Some(NumberBase::Octal), is_octal),
                Some("0d") => (Some(NumberBase::Decimal), is_digit),
                Some("0x") => (Some(NumberBase::Hexadecimal), is_hexadecimal),
                None => (None, is_digit),
                Some(pref) => return Err(LexerError::not_recognized_base_prefix(pref)),
            })
        }

        let prefix = process_prefix(self);
        let (string, is_floating, mode_parse_suffix) = process_number_body(self, prefix.is_some())?;
        let suffix = process_suffix(self, mode_parse_suffix);

        // Cursor part
        let _starts_with_dot = string.starts_with('.');
        let _ends_with_dot = string.ends_with('.');
        self.cursor.column += (string.len() + suffix.clone().unwrap_or_default().len() - 1) as u32;

        // Error handling
        let (number_base, is_in_number_base) = determine_number_base(&prefix)?;

        if prefix.is_some() && is_floating {
            return Err(LexerError::floats_dont_have_base_prefix(number_base));
        }
        if !string.chars().all(|c| is_in_number_base(&c)) {
            return Err(LexerError::invalid_digit_base_prefix(number_base));
        }

        let possible_suffixes = [
            "", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64",
        ];
        if !possible_suffixes.contains(&suffix.clone().unwrap_or_default().as_str()) {
            return Err(LexerError::invalid_digit_type_suffix(suffix));
        }
        assert_ne!(string.len(), 0);

        let lit = Literal(Number {
            content: string,
            prefix: number_base,
            suffix,
            is_float: is_floating,
        });

        Ok(lit)
    }

    fn lex_identifier(&mut self) -> LResult<TokenKind> {
        use LiteralKind::*;
        use TokenKind::*;

        let string_content = self.eat_while(is_alpha_numeric);

        // Here we determine whether the identifier is a keyword or not.

        let token = match string_content.as_ref() {
            lit if is_lit_bool(lit) => LiteralKind::from_str(lit).map_or_else(Identifier, Literal),
            kw => KeywordKind::from_str(kw).map_or_else(Identifier, Keyword),
        };
        match &token {
            Keyword(kw) => self.cursor.column += (kw.len() - 1) as u32,
            Identifier(id) => self.cursor.column += (id.len() - 1) as u32,
            Literal(Boolean(false)) => self.cursor.column += ("false".len() - 1) as u32,
            Literal(Boolean(true)) => self.cursor.column += ("true".len() - 1) as u32,
            other => panic!("Identifier not a keyword or identifier. {:?}", other),
        };
        Ok(token)
    }
}

/// This impl is the cursor part for the lexer.
///
///
impl Lexer {
    /// Advances the cursor by one.
    fn advance(&mut self) {
        self.advance_by(1);
    }

    /// Advances the cursor by the given offset.
    fn advance_by(&mut self, offset: usize) {
        self.index += offset;
    }

    /// Tries to return the index of the current index plus the given offset.
    ///
    /// If the offset doesn't overflow or underflow the `self.index`, then it will return the new index in a `Some`.
    fn try_index_plus(&self, offset: isize) -> Option<usize> {
        match offset.signum() {
            -1 => self.index.checked_sub(-offset as usize),
            1 => self.index.checked_add(offset as usize),
            0 => Some(self.index),
            _ => unreachable!("Signum of offset is not -1, 0 or 1"),
        }
    }

    /// Peeks the character at the current index plus the given offset.
    /// If the resulting location is valid, it returns the character wrapped in a `Some`, otherwise `None`.
    fn peek(&self, offset: isize) -> Option<char> {
        self.try_index_plus(offset)
            .and_then(|index| self.chars.get(index).copied())
    }

    fn is_eof(&self) -> bool {
        self.peek(0).is_none()
    }

    /// Peeks at `n + 1` characters starting from the current position and collects them into a string.
    ///
    /// An `Option` which is `None` if any of the peeks are invalid and `Some(String)` if all peeks are valid.
    fn peek_n_times_and_collect(&self, n: usize) -> Option<String> {
        (0..=n)
            .map(|i| self.peek(i as isize))
            .collect::<Option<String>>()
    }

    fn peek_into_str_2(&self, n: usize) -> Result<String, String> {
        (0..=n).try_fold(String::new(), |mut peeked, i| match self.peek(i as isize) {
            Some(c) => {
                peeked.push(c);
                Ok(peeked)
            }
            None => Err(peeked),
        })
    }

    /// Peeks at the offset `offset`. If the peeked char agrees with the given predicate, it returns it wrapped in a `Ok`,
    /// otherwise it returns the peeked char wrapped in a `Err`.
    fn check_with<F>(&self, offset: isize, func: F) -> Result<char, Option<char>>
    where
        Self: Sized,
        F: Fn(&char) -> bool,
    {
        self.peek(offset)
            .map(|c| if func(&c) { Ok(c) } else { Err(Some(c)) })
            .unwrap_or(Err(None))
    }

    /// Checks the next chars as long as the given function returns true.
    fn check_while<F>(&mut self, func: F) -> String
    where
        F: Fn(&char) -> bool,
    {
        (0..)
            .map(|offset| self.check_with(offset, &func))
            .take_while(Result::is_ok)
            .filter_map(Result::ok)
            .collect()
    }

    /// Checks the char at offset `n`. If the char is the same as the given char, it returns it wrapped in a `Some`, otherwise `None`.
    fn check_char(&self, c: char, n: isize) -> Result<char, Option<char>> {
        self.check_with(n, |x| x == &c)
    }

    fn check_str_(&self, str: &str) -> Option<String> {
        str.chars()
            .enumerate()
            .map(|(i, c)| self.check_char(c, i as isize).ok())
            .collect::<Option<String>>()
    }
    fn check_str(&self, str: &str) -> String {
        str.chars()
            .enumerate()
            .map(|(i, c)| self.check_char(c, i as isize).ok())
            .take_while(Option::is_some)
            .flatten()
            .collect::<String>()
    }

    /// Matches a terminal character. If the character is matched, it is eaten and return wrapped in a `Ok`,
    /// else an `Err(Some)` is returned if the character is not matched and an `Err(None)` is returned if the end of the file is reached.s
    fn eat_char(&mut self, ch: char) -> Result<char, Option<char>> {
        self.eat_with(|x| x == &ch)
    }

    /// Checks the charachter at the current index with a given function.
    /// If the function returns true, the current char is "eaten" and returned wrapped in a `Some`, otherwise `None`.
    fn eat_with<F>(&mut self, func: F) -> Result<char, Option<char>>
    where
        F: Fn(&char) -> bool,
    {
        self.check_with(0, &func).map(|char| {
            self.advance();
            char
        })
    }

    fn eat_while<F>(&mut self, func: F) -> String
    where
        F: Fn(&char) -> bool,
    {
        (0..)
            .map(|_| self.eat_with(&func))
            .take_while(Result::is_ok)
            .filter_map(Result::ok)
            .collect()
    }

    fn eat_str(&mut self, str: &str) -> Option<String> {
        str.chars()
            .map(|ch| self.eat_char(ch).ok())
            .collect::<Option<String>>()
    }
    fn _eat_char_any(&mut self, chars: &[char]) -> Option<char> {
        for ch in chars {
            if let Ok(c) = self.eat_char(*ch) {
                return Some(c);
            }
        }
        None
    }
}
#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use crate::token::{KeywordKind, LiteralKind, PunctuatorKind, TokenKind};

    mod cursor {
        use super::*;
        #[test]
        fn test_peek() {
            let txt = "a";
            let lexer = Lexer::new_from_str(txt);
            assert_eq!(lexer.peek(0), Some('a'));
            assert_eq!(lexer.peek(1), None);

            let txt = "abc";
            let mut lexer = Lexer::new_from_str(txt);
            lexer.advance();
            assert_eq!(lexer.peek(0), Some('b'));
            assert_eq!(lexer.peek(1), Some('c'));
            assert_eq!(lexer.peek(-1), Some('a'));
            assert_eq!(lexer.peek(-2), None);
            assert_eq!(lexer.peek(2), None);
            assert_eq!(lexer.peek(-3), None);
            assert_eq!(lexer.peek(3), None);
        }

        #[test]
        fn test_check_with() {
            let txt = "a";
            let lexer = Lexer::new_from_str(txt);
            assert_eq!(lexer.check_with(0, |x| x == &'a'), Ok('a'));
            assert_eq!(lexer.check_with(0, |x| x == &'b'), Err(Some('a')));
            assert_eq!(lexer.check_with(1, |x| x == &'a'), Err(None));
            assert_eq!(lexer.check_with(1, |x| x == &'b'), Err(None));
            assert_eq!(lexer.check_with(-1, |x| x == &'a'), Err(None));
            assert_eq!(lexer.check_with(-1, |x| x == &'b'), Err(None));

            let txt = "abc";
            let mut lexer = Lexer::new_from_str(txt);
            assert_eq!(lexer.check_with(0, |x| x == &'a'), Ok('a'));
            assert_eq!(lexer.check_with(1, |x| x == &'b'), Ok('b'));
            assert_eq!(lexer.check_with(2, |x| x == &'c'), Ok('c'));
            assert_eq!(lexer.check_with(3, |x| x == &'d'), Err(None));
            assert_eq!(lexer.check_with(-1, |x| x == &'a'), Err(None));
            assert_eq!(lexer.check_with(-2, |x| x == &'b'), Err(None));
            assert_eq!(lexer.check_with(-3, |x| x == &'c'), Err(None));
            assert_eq!(lexer.check_with(-4, |x| x == &'d'), Err(None));
            lexer.advance();
            assert_eq!(lexer.check_with(0, |x| x == &'b'), Ok('b'));
            assert_eq!(lexer.check_with(1, |x| x == &'c'), Ok('c'));
            assert_eq!(lexer.check_with(2, |x| x == &'d'), Err(None));
            assert_eq!(lexer.check_with(-1, |x| x == &'a'), Ok('a'));
            assert_eq!(lexer.check_with(-2, |x| x == &'b'), Err(None));
        }

        #[test]
        fn test_check_while() {
            let txt = "abc504";
            let mut lexer = Lexer::new_from_str(txt);
            assert_eq!(lexer.check_while(|x| x == &'a'), "a".to_string());
            assert_eq!(lexer.check_while(|x| x.is_alphabetic()), "abc".to_string());
            assert_eq!(
                lexer.check_while(|x| x.is_alphanumeric()),
                "abc504".to_string()
            );
            assert_eq!(lexer.check_while(|x| x.is_numeric()), "".to_string());
            let txt = "123four";
            let mut lexer = Lexer::new_from_str(txt);
            assert_eq!(lexer.check_while(|x| x.is_numeric()), "123".to_string());
            assert_eq!(lexer.check_while(|x| x.is_alphabetic()), "".to_string());
            assert_eq!(
                lexer.check_while(|x| x.is_alphanumeric()),
                "123four".to_string()
            );
        }

        #[test]
        fn test_check_str() {
            let txt = "hello";
            let lexer = Lexer::new_from_str(txt);
            assert_eq!(lexer.check_str("hello"), "hello".to_owned());
            assert_eq!(lexer.check_str_("hEllo"), None);

            let txt = "";
            let lexer = Lexer::new_from_str(txt);
            assert_eq!(lexer.check_str("hello"), "".to_owned());
        }

        #[test]
        fn test_eat_with() {
            let txt = "a";
            let mut lexer = Lexer::new_from_str(txt);
            assert_eq!(lexer.eat_with(|x| x == &'b'), Err(Some('a')));
            assert_eq!(lexer.eat_with(|x| x == &'a'), Ok('a'));
            assert_eq!(lexer.eat_with(|x| x == &'b'), Err(None));
            assert_eq!(lexer.eat_with(|x| x == &'a'), Err(None));

            let txt = "abc";
            let mut lexer = Lexer::new_from_str(txt);
            assert_eq!(lexer.eat_with(|x| x == &'a'), Ok('a'));
            assert_eq!(lexer.eat_with(|x| x == &'b'), Ok('b'));
            assert_eq!(lexer.eat_with(|x| x == &'c'), Ok('c'));
            assert_eq!(lexer.eat_with(|x| x == &'d'), Err(None));
            assert_eq!(lexer.eat_with(|x| x == &'a'), Err(None));
            assert_eq!(lexer.eat_with(|x| x == &'b'), Err(None));
        }
        #[test]
        fn test_eat_while() {
            let txt = "abc504";
            let mut lexer = Lexer::new_from_str(txt);
            assert_eq!(lexer.eat_while(|x| x == &'a'), "a".to_string());
            assert_eq!(lexer.eat_while(|x| x.is_alphabetic()), "bc".to_string());
            assert_eq!(lexer.eat_while(|x| x.is_alphanumeric()), "504".to_string());
            assert_eq!(lexer.eat_while(|x| x.is_numeric()), "".to_string());
        }
        #[test]
        fn test_eat_str() {
            let txt = "abc";
            let mut lexer = Lexer::new_from_str(txt);
            assert_eq!(lexer.eat_str("bc"), None);
            assert_eq!(lexer.eat_str("ab"), Some("ab".to_string()));
            assert_eq!(lexer.eat_str("bc"), None);
            assert_eq!(lexer.eat_str("c"), Some("c".to_string()));
        }
        #[test]
        fn test_peek_n_times_and_collect() {
            let txt = "abc";
            let lexer = Lexer::new_from_str(txt);
            assert_eq!(lexer.peek_n_times_and_collect(2), Some("abc".to_string()));
            assert_eq!(lexer.peek_n_times_and_collect(1), Some("ab".to_string()));
            assert_eq!(lexer.peek_n_times_and_collect(0), Some("a".to_string()));
            assert_eq!(lexer.peek_n_times_and_collect(4), None);
        }
        #[test]
        fn test_peek_into_str_2() {
            let txt = "abc";
            let lexer = Lexer::new_from_str(txt);
            assert_eq!(lexer.peek_into_str_2(2), Ok("abc".to_string()));
            assert_eq!(lexer.peek_into_str_2(1), Ok("ab".to_string()));
            assert_eq!(lexer.peek_into_str_2(0), Ok("a".to_string()));
            assert_eq!(lexer.peek_into_str_2(4), Err("abc".to_string()));
        }
    }

    #[test]
    fn test_lex_char() {
        use crate::lexer::LexerError;
        use LiteralKind::*;
        use TokenKind::*;
        let txt = r#"'a'"#;
        let mut lexer = Lexer::new_from_str(txt);
        assert_eq!(lexer.lex_char(), Ok(Literal(Char('a'))));

        // Handling errors
        let txt = r#"''"#;
        let mut lexer = Lexer::new_from_str(txt);
        assert_eq!(lexer.lex_char(), Err(LexerError::empty_char_literal()));

        let txt = r#"'
        '"#;
        let mut lexer = Lexer::new_from_str(txt);
        assert_eq!(lexer.lex_char(), Err(LexerError::new_line_in_char_lit()));

        let txt = r#"'ab'"#;
        let mut lexer = Lexer::new_from_str(txt);
        assert_eq!(
            lexer.lex_char(),
            Err(LexerError::char_lit_contains_multiple_codepoints())
        );

        let txt = r#"'\"#;
        let mut lexer = Lexer::new_from_str(txt);
        assert_eq!(lexer.lex_char(), Err(LexerError::unterminated_char_lit()));
    }
    #[test]
    fn comment_line_0() {
        let txt = "//";
        let _ = Lexer::new_from_str(txt).scan_tokens();
    }
    #[test]
    fn comment_line_1() {
        let txt = "// This is a comment";
        let _ = Lexer::new_from_str(txt).scan_tokens();
    }
    #[test]
    fn comment_line_2() {
        let txt = "// This is a comment\n// And this too\n";
        let _ = Lexer::new_from_str(txt).scan_tokens();
    }
    #[test]
    fn comment_block_0() {
        let txt = "/* This is a line comment*/";
        let _ = Lexer::new_from_str(txt).scan_tokens();
    }
    #[test]
    #[should_panic]
    fn comment_block_1() {
        let txt = "/*";
        let _ = Lexer::new_from_str(txt).scan_tokens();
    }

    #[test]
    fn num_0_() {
        let txt = "0";
        let _ = Lexer::new_from_str(txt).scan_tokens();
    }

    #[test]
    #[should_panic]
    fn num_0() {
        let txt = "0x0.0";
        let _ = Lexer::new_from_str(txt).scan_tokens();
    }

    mod num {
        use super::*;

        #[test]
        #[should_panic]
        fn invalid_binary() {
            let txt = "0b444";
            let _ = Lexer::new_from_str(txt).scan_tokens();
        }
        #[test]
        #[should_panic]
        fn invalid_octal() {
            let txt = "0o99";
            let _ = Lexer::new_from_str(txt).scan_tokens();
        }

        #[test]
        #[should_panic]
        fn invalid_float() {
            let txt = "0.";
            let t = Lexer::new_from_str(txt).scan_tokens();
            assert_ne!(t.len() - 1, 1);
        }

        #[test]
        fn t1() {
            use PunctuatorKind::*;
            use TokenKind::*;
            let txt = ".0";
            let t = Lexer::new_from_str(txt).scan_tokens();
            assert_eq!(t.len() - 1, 2);
            assert_eq!(t[0].kind, Punctuator(Dot));
        }
    }
    #[test]
    fn init_colonequal() {
        use KeywordKind::*;
        use LiteralKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        let source = "var a := 2222;";

        let mut lexer = Lexer::new_from_str(source);
        let tokens = lexer.scan_tokens();

        assert_eq!(tokens[0].kind, Keyword(Var));
        assert_eq!(tokens[1].kind, Identifier(String::from("a")));
        assert_eq!(tokens[2].kind, Punctuator(Colon));
        assert_eq!(tokens[3].kind, Punctuator(Equal));
        assert_eq!(
            tokens[4].kind,
            Literal(Number {
                content: "2222".to_owned(),
                prefix: None,
                suffix: None,
                is_float: false,
            })
        );
        assert_eq!(tokens[5].kind, Punctuator(Semicolon));
    }
    #[test]
    fn coloncolon() {
        use PunctuatorKind::*;
        use TokenKind::*;

        let source = "std::io::str;";
        let mut lexer = Lexer::new_from_str(source);
        let tokens = lexer.scan_tokens();
        assert_eq!(tokens[0].kind, Identifier(String::from("std")));
        assert_eq!(tokens[1].kind, Punctuator(ColonColon));
        assert_eq!(tokens[2].kind, Identifier(String::from("io")));
        assert_eq!(tokens[3].kind, Punctuator(ColonColon));
        assert_eq!(tokens[4].kind, Identifier(String::from("str")));
        assert_eq!(tokens[5].kind, Punctuator(Semicolon));
    }
}
