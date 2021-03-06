//! This module contains the lexer.
//!
//! TODO:
//!
//! - [ ] Maybe in the future, this module should be elevated into one's own crate.
//! - [ ] Make it possible to be an on demand lexer.
//! - [ ] Implement nested comments.
//! - [ ] Consider adding different comment styles.

use std::convert::TryInto;

use crate::error::*;
use crate::token;
use crate::token::*;

fn is_digit(c: char) -> bool {
    ('0'..='9').contains(&c)
}
fn is_hexadecimal(c: char) -> bool {
    let num = ('0'..='9').contains(&c);
    let upper = ('A'..='F').contains(&c);
    let lower = ('a'..='f').contains(&c);
    num || upper || lower
}
fn is_octal(c: char) -> bool {
    ('0'..='7').contains(&c)
}
fn is_binary(c: char) -> bool {
    ('0'..='1').contains(&c)
}
fn is_alpha(c: char) -> bool {
    let minor_case = ('a'..='z').contains(&c);
    let major_case = ('A'..='Z').contains(&c);
    let underscore = c == '_';
    minor_case || major_case || underscore
}
fn is_alpha_numeric(c: char) -> bool {
    is_digit(c) || is_alpha(c)
}
// fn is_whitespace(c: char) -> bool {
//     c == ' ' || c == '\t' || c == '\n' || c == '\r'
// }

fn is_left_whitespace(c: char) -> bool {
    c == ' '
}
fn is_right_whitespace(c: char) -> bool {
    c == ' '
}
#[allow(dead_code)]
pub fn get_tokens_from_source(source: &str) -> Vec<Token> {
    Lexer::new_from_str(source).scan_tokens()
}

struct Lexer {
    chars: Vec<char>,

    index: usize,
    cursor: Position,
}

impl Lexer {
    fn new_from_str(source: &str) -> Self {
        Self {
            chars: source.chars().collect(),
            index: 0,
            cursor: Position::new(1, 1),
        }
    }
    fn scan_tokens(&mut self) -> Vec<Token> {
        let mut puncs: Vec<Token> = Vec::new();
        let mut tokens = Vec::new();
        while let Some(ch) = self.peek(0) {
            let token = self.scan_token(ch);
            println!("{:?}", &token);
            use PunctuatorKind::*;
            use TokenKind::*;

            // In this part we combine simple punctuator tokens to complex ones, if possible

            // We have a `puncs`, where copies of the previous punctuators lie,
            // which could be combined to a complex token.

            // If the scanned token is not a punctuator, we flush the `puncs`.
            //let is_empty = puncs.is_empty();
            // let mut to_puncs = false;
            // let mut to_tokens = false;

            let last_puncs_kind = {
                if !puncs.is_empty() {
                    Some(puncs[puncs.len() - 1].kind.clone())
                } else {
                    None
                }
            };
            let is_last_column = last_puncs_kind == Some(Punctuator(Colon));

            if let Punctuator(punc) = &token.kind {
                let is_last_same = last_puncs_kind == Some(Punctuator(punc.clone()));
                let is_curr_equal = punc == &Equal;

                let is_column_equal = is_last_column && is_curr_equal;

                if punc.is_structural() {
                    // Generally speaking, there are special rules for combining structural punctuators into complex puncturators.
                    // `.`: Must be the first punctuator OR all the preceding punctuators must be `.`.
                    // - valid examples: `.`, `.=`, `..`
                    // - invalid examples: `=.`, `.=.`
                    // `:`: Must be the first punctuator OR all the preceding punctuators must be `:`
                    // - valid examples: `:`, `::`
                    // - invalid examples: `:=:`
                    // `=`: It can't form a complex token with a preceding `:`.

                    if [Dot, Colon].contains(punc) {
                        if (puncs.is_empty() || is_last_same) && is_column_equal {
                            puncs.push(token.clone());
                        }
                    }
                    tokens.push(token);
                }
            } else {
                if !puncs.is_empty() {
                    tokens.push(cook_tokens(&puncs));
                    puncs.clear();
                }
                if !token.kind.is_to_skip() {
                    tokens.push(token);
                }
            }
        }
        if !puncs.is_empty() {
            tokens.push(cook_tokens(&puncs));
            puncs.clear();
        }

        use SpecialKeywordKind::*;
        use TokenKind::*;
        let token_eof = Token::new(
            SpecialKeyword(Eof),
            Span::from_tuples((1, 1), (1, 1)),
            token::Whitespace::None,
        );
        tokens.push(token_eof);
        tokens
    }

    fn scan_token_kind(&mut self, c: char) -> Result<TokenKind, LexerError> {
        use PunctuatorKind::*;
        use TokenKind::*;

        let kind = match c {
            '/' => match self.peek_into_str(1).unwrap().as_str() {
                "//" => self.lex_comment_line(),
                "/*" => self.lex_comment_block(),
                _ => Ok(Punctuator(Slash)),
            },
            '.' => match self.peek(1) {
                Some(s) if is_digit(s) && !self.check_at('.', -1).ok().is_some() => {
                    self.lex_number()
                }
                _ => Ok(Punctuator(Dot)),
            },
            '"' => self.lex_string(),
            '\'' => self.lex_char(),
            p if PunctuatorKind::from_char(p).is_some() => {
                Ok(Punctuator(PunctuatorKind::from_char(p).unwrap()))
            }
            p if SpecialKeywordKind::from_char(p).is_some() => {
                Ok(SpecialKeyword(SpecialKeywordKind::from_char(p).unwrap()))
            }
            cc if is_digit(cc) => self.lex_number(),
            cc if is_alpha(cc) => self.lex_identifier(),
            cc => match cc {
                _ => Err(LexerError::new(format!(
                    "Character '{}' is unknown. At '{}:{}'.",
                    cc, self.cursor.line, self.cursor.column
                ))),
            },
        };
        let kind = kind?;
        // NOTE: If the token is simple we advance here, because the match statements look prettier that way.
        // With more complex tokens, basically everything except single punctuators,
        // handle the advance in their own functions
        if kind.is_simple() {
            self.advance();
        }

        Ok(kind)
    }

    /// Scans a token
    /// Returns the token which was lexed. It also moves the 'cursor' to the next character, so that one can lex the next token.
    fn scan_token(&mut self, c: char) -> Token {
        use SpecialKeywordKind::*;
        let start_pos = self.cursor;
        let start_index = self.index;

        let token_kind = self.scan_token_kind(c).unwrap();

        let end_pos = self.cursor;
        let end_index = self.index;

        if token_kind == TokenKind::SpecialKeyword(Newline) {
            self.cursor.line += 1;
            self.cursor.column = 1;
        } else {
            self.cursor.column += 1;
        }

        let whitespace = {
            let index_start = 0 - (end_index - start_index) as isize;
            let index_end = -1 as isize;

            let index_start_left = self.peek(index_start - 1);
            let index_end_right = self.peek(index_end + 1);

            let ws_left = is_left_whitespace(index_start_left.unwrap_or(' '));
            let ws_right = is_right_whitespace(index_end_right.unwrap_or(' '));

            token::Whitespace::from((ws_left, ws_right))
        };

        Token::new(token_kind, Span::new(start_pos, end_pos), whitespace)
    }
    fn lex_escape_char(&mut self) -> Result<char, LexerError> {
        self.eat('\\').unwrap();
        self.cursor.column += 1;

        let escaped_char = match self.peek(0).unwrap() {
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
            '\\' => Ok('\\'),
            '\'' => Ok('\''),
            '"' => Ok('"'),
            '0' => Ok('\0'),
            _ => Err(LexerError::new(format!("Unknown escape sequence"))),
        };
        self.advance();
        self.cursor.column += 1;

        self.cursor.column -= 1;

        escaped_char
    }
    fn lex_char(&mut self) -> Result<TokenKind, LexerError> {
        use LiteralKind::*;
        use TokenKind::*;
        self.eat('\'').unwrap();

        let c = if let Some(c) = self.peek(0) {
            let c = match c {
                '\\' => self.lex_escape_char(),
                '\'' => Err(LexerError::new(format!("empty character literal"))),
                '\n' | '\r' => Err(LexerError::new(format!("newline in character literal"))),
                _ => {
                    self.advance();
                    self.cursor.column += 1;
                    Ok(c)
                }
            };
            c.ok()
        } else {
            None
        };

        match self.eat('\'') {
            Some(_) => {}
            None => {
                return Err(LexerError::new(format!(
                    "character literal may only contain one codepoint"
                )))
            }
        };
        Ok(Literal(Char(c.unwrap())))
    }
    fn lex_string(&mut self) -> Result<TokenKind, LexerError> {
        use LiteralKind::*;
        use TokenKind::*;
        self.eat('\"').unwrap();
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
        self.eat('\"').unwrap();
        self.cursor.column += 1;

        self.cursor.column -= 1;

        if self.peek(0) == None {
            return Err(LexerError::new(format!("Unterminated string.")));
        }
        Ok(Literal(Str(string_content)))
    }
    fn lex_comment_line(&mut self) -> Result<TokenKind, LexerError> {
        use SpecialKeywordKind::*;
        use TokenKind::*;
        // TODO: Implement that a comment at the end of a file is possible, meanign when it does not end through a newline, but eof
        self.eat_str("//").unwrap();
        self.cursor.column += 2;

        while self.peek(0) != Some('\n') && self.peek(0) != None {
            self.advance();
            self.cursor.column += 1;
        }
        self.cursor.column -= 1;
        //assert!(self.peek(0) == Some('\n'));
        Ok(SpecialKeyword(Comment))
    }
    fn lex_comment_block(&mut self) -> Result<TokenKind, LexerError> {
        use SpecialKeywordKind::*;
        use TokenKind::*;
        // TODO: Implement nested block comments
        self.eat_str("/*").unwrap();
        self.cursor.column += 2;

        while !self.check_str("*/").is_some() {
            self.advance();

            if self.peek(0) == Some('\n') {
                self.cursor.line += 1;
                self.cursor.column = 1;
            } else {
                self.cursor.column += 1;
            }
        }

        self.eat_str("*/").unwrap();
        self.cursor.column += 2;

        self.cursor.column -= 1;

        Ok(SpecialKeyword(Comment))
    }

    fn lex_number(&mut self) -> Result<TokenKind, LexerError> {
        use LiteralKind::*;
        use TokenKind::*;

        // TODO: Floating point numbers can't be abbreviated like in some other languages.
        // `.5`, `5.` are not possible short forms for `0.5` and `5.0` respectively.

        let (number_base, has_base_prefix, is_in_number_base): (
            NumberBase,
            bool,
            fn(char) -> bool,
        ) = match self.peek_into_str(1).unwrap().as_str() {
            "0b" => (NumberBase::Binary, true, is_binary),
            "0o" => (NumberBase::Octal, true, is_octal),
            "0d" => (NumberBase::Decimal, true, is_digit),
            "0x" => (NumberBase::Hexadecimal, true, is_hexadecimal),
            _ => (NumberBase::Decimal, false, is_digit),
        };
        if has_base_prefix {
            self.advance();
            self.advance();
            self.cursor.column += 1;
            self.cursor.column += 1;
        }
        let mut is_after_dot = false;
        let mut is_floating = false;
        let mut string = String::new();
        while let Some(c) = self.peek(0) {
            let res: Result<(), LexerError> = match c {
                // checks whether it is a possible digit at all.
                c if is_hexadecimal(c) => {
                    if is_in_number_base(c) {
                        string.push(c);
                        if is_after_dot {
                            is_after_dot = false;
                        }
                        Ok(())
                    } else {
                        Err(LexerError::new(format!(
                            "invalid digit for base {} literal",
                            number_base.as_num()
                        )))
                    }
                }
                // in a number there can be only one dot
                // it can't be in a number which has a prefix, because floating points don't have a base prefix
                '.' if !is_floating
                    && !has_base_prefix
                    && !self.check_at('.', 1).ok().is_some() =>
                {
                    string.push(c);
                    is_floating = true;
                    is_after_dot = true;
                    Ok(())
                }
                // a '_' is not possible after a '.'. E.g. '1._3' is not a number, this is an integer followed by an identifier.
                '_' if !is_after_dot => Ok(()),
                '.' if is_floating => Err(LexerError::new(format!("multiple dots in number"))),
                '.' if has_base_prefix => Err(LexerError::new(format!("dot after base prefix"))),

                _ => break,
            };
            if let Err(e) = res {
                return Err(e);
            } else {
                self.advance()
            }
        }

        let _starts_with_dot = string.starts_with('.');
        let _ends_with_dot = string.ends_with('.');
        self.cursor.column += (string.len() - 1) as u32;

        let lit = if is_floating {
            Literal(Floating { content: string })
        } else {
            Literal(Integer {
                content: string,
                base: number_base,
            })
        };
        Ok(lit)
    }
    fn lex_identifier(&mut self) -> Result<TokenKind, LexerError> {
        use KeywordKind::*;
        use LiteralKind::*;
        use TokenKind::*;

        let mut string_content = String::new();
        while let Some(ch) = self.peek(0) {
            if is_alpha_numeric(ch) {
                string_content.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        // Here we determine whether the identifier is a keyword or not.

        let token = match string_content.as_ref() {
            "var" => Keyword(Var),
            "fn" => Keyword(Fn),
            "if" => Keyword(If),
            "else" => Keyword(Else),
            "while" => Keyword(While),
            "for" => Keyword(For),

            "print" => Keyword(Print),
            "println" => Keyword(Println),

            "true" => Literal(Boolean(true)),
            "false" => Literal(Boolean(false)),
            _ => Identifier(string_content),
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
    fn move_by(&mut self, offset: isize) {
        //let s: usize = self.index + offset.try_into().unwrap();
        //self.index += self.calc_offset(n);
    }
    fn advance(&mut self) {
        self.index += 1;
    }
    fn advance_new_line(&mut self) {
        self.index += 1;
        self.cursor.line += 1;
        self.cursor.column = 1;
    }

    fn calc_offset(&self, offset: isize) -> Option<usize> {
        if offset.is_negative() {
            self.index.checked_sub(-offset as usize)
        } else if offset.is_positive() {
            self.index.checked_add(offset as usize)
        } else {
            Some(self.index)
        }
    }
    /// This function peeks the current character at the current location of the cursor plus the given offset.
    /// If the offset is negative, it will look at the previous character.
    /// If the offset is positive, it will look at the next character.
    /// If the offset is 0, it will look at the current character.
    /// If resulting index is out of bounds, it will return None.
    fn peek(&self, offset: isize) -> Option<char> {
        match self.calc_offset(offset) {
            Some(new_offset) => self.chars.get(new_offset).copied(),
            None => None,
        }
    }
    fn is_inbounds(&self, offset: isize) -> bool {
        match self.calc_offset(offset) {
            Some(new_offset) => new_offset < self.chars.len(),
            None => false,
        }
    }
    /// peeks up to `n` consecutive chars and returns them as a string. If 'n == 0', refers to the current
    fn peek_into_str(&self, n: usize) -> Option<String> {
        if self.is_inbounds(n as isize) {
            let mut str = String::new();
            for i in 0..=n {
                let c = self.peek(i as isize).unwrap();
                str.push(c);
            }
            Some(str)
        } else {
            None
        }
    }

    /// peeks at the offset `n`. If the peeked char is the same as `c`, it returns it wrapped in a `Some`, otherwise `None`.
    fn check_at(&self, c: char, n: isize) -> Result<char, Option<char>> {
        match self.peek(n) {
            Some(ch) if c == ch => Ok(c),
            Some(other) => Err(Some(other)),
            None => Err(None),
        }
    }
    fn check(&self, c: char) -> Result<char, Option<char>> {
        self.check_at(c, 0)
    }
    fn check_str(&self, s: &str) -> Option<String> {
        if self.peek_into_str(s.len() - 1) == Some(s.to_string()) {
            Some(s.to_string())
        } else {
            None
        }
    }
    /// Matches a terminal character. If the character is matched, an Option with that character is returned, otherwise None.
    fn eat(&mut self, ch: char) -> Option<char> {
        let c = self.check(ch);
        if c.is_ok() {
            self.advance();
        }
        c.ok()
    }
    fn eat_str(&mut self, str: &str) -> Option<String> {
        let mut result = String::new();
        let chars = str.chars().collect::<Vec<char>>();
        for n in 0..chars.len() {
            let c = self.peek(n as isize).unwrap();
            result.push(c);
        }
        if result == str {
            for _ in 0..chars.len() {
                self.advance();
            }
            Some(result)
        } else {
            None
        }
    }
    fn eat_char_any(&mut self, chars: &[char]) -> Option<char> {
        for ch in chars {
            if let Some(c) = self.eat(*ch) {
                return Some(c);
            }
        }
        None
    }
}
#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use crate::token::{KeywordKind, LiteralKind, NumberBase, PunctuatorKind, TokenKind};

    #[test]
    fn test_0() {
        let source = "var a = 2222;";

        let mut lexer = Lexer::new_from_str(source);
        let tokens = lexer.scan_tokens();

        assert_eq!(tokens[0].kind, TokenKind::Keyword(KeywordKind::Var));
        assert_eq!(tokens[1].kind, TokenKind::Identifier(String::from("a")));
        assert_eq!(tokens[2].kind, TokenKind::Punctuator(PunctuatorKind::Equal));
        assert_eq!(
            tokens[3].kind,
            TokenKind::Literal(LiteralKind::Integer {
                content: "2222".to_owned(),
                base: NumberBase::Decimal,
            })
        );
        assert_eq!(
            tokens[4].kind,
            TokenKind::Punctuator(PunctuatorKind::Semicolon)
        );
    }
}
