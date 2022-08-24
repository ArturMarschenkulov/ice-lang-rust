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

fn is_digit(c: &char) -> bool {
    ('0'..='9').contains(c)
}
fn is_hexadecimal(c: &char) -> bool {
    // let is_hd = c.is_digit(16);
    // let is_lower_case = c.is_uppercase();
    // is_hd && is_lower_case

    let num = ('0'..='9').contains(c);
    let upper = ('A'..='F').contains(c);
    let lower = ('a'..='f').contains(c);
    num || upper || lower
}
fn is_octal(c: &char) -> bool {
    // c.is_digit(8)
    ('0'..='7').contains(c)
}
fn is_binary(c: &char) -> bool {
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

pub fn lex_tokens_from_file(source: &str) -> LResult<Vec<Token>> {
    let tokens = Lexer::new_from_str(source).scan_tokens();
    Ok(tokens)
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

            // We have a `punc_cache`, where copies of the previous punctuators lie,
            // which could be combined to a complex token.

            // If the scanned token is not a punctuator, we flush the `punc_cache`.

            // Generally speaking, there are special rules for combining structural punctuators into complex puncturators.
            // `.`: Must be the first punctuator OR all the preceding punctuators must be `.`.
            // - valid examples: `.`, `.=`, `..`
            // - invalid examples: `=.`, `.=.`
            // `:`: Must be the first punctuator OR all the preceding punctuators must be `:`
            // - valid examples: `:`, `::`
            // - invalid examples: `:=:`
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

        let token_eof = Token::new(
            SpecialKeyword(Eof),
            token::Span::from_tuples((1, 1), (1, 1)),
            token::Whitespace::None,
        );
        tokens.push(token_eof);
        tokens
    }

    fn scan_token_kind(&mut self, c: char) -> LResult<(TokenKind, token::Position, usize)> {
        use PunctuatorKind::*;
        use SpecialKeywordKind::*;
        use TokenKind::*;

        let index_old = self.index;
        let kind = match c {
            '/' => match self.peek_into_str(1).unwrap().as_str() {
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
            cc => match cc {
                _ => Err(LexerError::new(format!(
                    "Character '{}' is unknown. At '{}:{}'.",
                    cc, self.cursor.line, self.cursor.column
                ))),
            },
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

            let index_start_left = self.peek(index_start - 1);
            let index_end_right = self.peek(index_end + 1);

            let ws_left = is_left_whitespace(&index_start_left.unwrap_or(' '));
            let ws_right = is_right_whitespace(&index_end_right.unwrap_or(' '));

            token::Whitespace::from((ws_left, ws_right))
        };

        let token = Token::new(token_kind, token::Span::new(start_pos, end_pos), whitespace);
        Ok(token)
    }
    fn lex_escape_char(&mut self) -> LResult<char> {
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
    fn lex_char(&mut self) -> LResult<TokenKind> {
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
            Ok(..) => {}
            Err(..) => {
                return Err(LexerError::new(format!(
                    "character literal may only contain one codepoint"
                )))
            }
        };
        Ok(Literal(Char(c.unwrap())))
    }
    fn lex_string(&mut self) -> LResult<TokenKind> {
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
    fn lex_comment_line(&mut self) -> LResult<TokenKind> {
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
        Ok(SpecialKeyword(Comment(CommentKind::Line)))
    }
    fn lex_comment_block(&mut self) -> LResult<TokenKind> {
        use SpecialKeywordKind::*;
        use TokenKind::*;
        // TODO: Implement nested block comments
        self.eat_str("/*").unwrap();
        self.cursor.column += 2;

        while self.check_str("*/").is_none() {
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

        Ok(SpecialKeyword(Comment(CommentKind::Block)))
    }

    fn lex_number(&mut self) -> LResult<TokenKind> {
        use LiteralKind::*;
        use TokenKind::*;

        // A number literal can be divided up into three parts.
        // An optional prefix, a number base: It shows which radix the number is in.
        // A number body: The actual number.
        // An optional suffix: It shows the type of the number, like f32, u8, etc.

        // The idea is to at first parse those three parts and then look wether they match the rules.

        // TODO: Try to make it possible that the lexer does not parse `foo.0.0` as a floating point
        // TODO: Maybe do not make the number base hard coded similar how suffixes are not? Only a consideration.

        // Prefix Part
        // let first_two_chars = self.peek_into_str(1).unwrap().clone();

        let prefix = match self.peek_into_str(1) {
            Some(c) if ["0b", "0o", "0d", "0x"].contains(&c.as_str()) => {
                self.advance();
                self.advance();
                self.cursor.column += 2;
                Some(c.clone())
            }
            _ => None,
        };
        let (number_base, is_in_number_base): (Option<NumberBase>, fn(&char) -> bool) =
            match prefix.as_ref().map(|ref s| s.as_str()) {
                Some("0b") => (Some(NumberBase::Binary), is_binary),
                Some("0o") => (Some(NumberBase::Octal), is_octal),
                Some("0d") => (Some(NumberBase::Decimal), is_digit),
                Some("0x") => (Some(NumberBase::Hexadecimal), is_hexadecimal),
                None => (None, is_digit),
                _ => unreachable!(),
            };

        // Number Body Part
        let mut is_after_dot = false;
        let mut is_floating = false;
        let mut mode_parse_suffix = false;
        let mut string = String::new();
        let allow_letters = number_base == Some(NumberBase::Hexadecimal);
        while let Some(c) = self.peek(0) {
            let res: LResult<()> = match c {
                // checks whether it is a possible digit at all.
                c if (allow_letters && is_hexadecimal(&c)) || (!allow_letters && is_digit(&c)) => {
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
                '.' if !is_floating && self.check('.', 1).ok().is_none() => {
                    string.push(c);
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
                self.advance()
            }
        }
        // Suffix part
        let suffix = if mode_parse_suffix {
            let mut suffix: String = String::new();
            while self.peek(0).unwrap().is_alphanumeric() {
                suffix.push(self.peek(0).unwrap());
                self.advance();
            }
            Some(suffix)
        } else {
            None
        };

        // Cursor part
        let _starts_with_dot = string.starts_with('.');
        let _ends_with_dot = string.ends_with('.');
        self.cursor.column +=
            (string.len() + suffix.clone().unwrap_or(String::new()).len() - 1) as u32;

        // Error handling
        if prefix.is_some() && is_floating {
            return Err(LexerError::new(format!(
                "floating point literals are not allowed to have a base prefix (`{}`)",
                number_base.unwrap_or(NumberBase::Decimal).as_num()
            )));
        }
        if !string.chars().all(|c| is_in_number_base(&c)) {
            return Err(LexerError::new(format!(
                "invalid digit for base {} literal",
                number_base.unwrap_or(NumberBase::Decimal).as_num()
            )));
        }

        let possible_suffixes = [
            "", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64",
        ];
        if !possible_suffixes.contains(&suffix.clone().unwrap_or("".to_string()).as_str()) {
            return Err(LexerError::new(format!(
                "invalid suffix `{}` for numeric literal",
                suffix.unwrap_or("".to_string())
            )));
        }

        let lit = if is_floating {
            Literal(Floating {
                content: string,
                suffix,
            })
        } else {
            Literal(Integer {
                content: string,
                base: number_base,
                suffix,
            })
        };
        Ok(lit)
    }
    fn lex_identifier(&mut self) -> LResult<TokenKind> {
        use LiteralKind::*;
        use TokenKind::*;

        let mut string_content = String::new();
        while let Some(ch) = self.peek(0) {
            if is_alpha_numeric(&ch) {
                string_content.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        loop {
            match self.peek(0) {
                Some(ch) if is_alpha_numeric(&ch) => {
                    string_content.push(ch);
                    self.advance();
                }
                _ => break,
            }
        }
        // TODO: Here we are trying to do the above in a more functional way
        {
            let _peeked = self.peek(0).map(|x| x.is_alphanumeric());
            //println!("jnklsrnlkgn {:?}", s);
        }

        // Here we determine whether the identifier is a keyword or not.

        let token = match string_content.as_ref() {
            "true" => Literal(Boolean(true)),
            "false" => Literal(Boolean(false)),
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
    // fn move_by(&mut self, offset: isize) {
    //     //let s: usize = self.index + offset.try_into().unwrap();
    //     //self.index += self.calc_offset(n);
    // }
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
    // fn peek(&self, offset: isize) -> Option<&char> {
    //     match self.calc_offset(offset) {
    //         Some(new_offset) => self.chars.get(new_offset),
    //         None => None,
    //     }
    // }
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
    fn check(&self, c: char, n: isize) -> Result<char, Option<char>> {
        match self.peek(n) {
            Some(ch) if c == ch => Ok(c),
            Some(other) => Err(Some(other)),
            None => Err(None),
        }
    }
    fn check_str(&self, s: &str) -> Option<String> {
        if self.peek_into_str(s.len() - 1) == Some(s.to_string()) {
            Some(s.to_string())
        } else {
            None
        }
    }
    /// Matches a terminal character. If the character is matched, an Option with that character is returned, otherwise None.
    fn eat(&mut self, ch: char) -> Result<char, Option<char>> {
        let c = self.check(ch, 0);
        if c.is_ok() {
            self.advance();
        }
        c
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
            if let Ok(c) = self.eat(*ch) {
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
    #[test]
    #[should_panic]
    fn num_1() {
        let txt = "0b444";
        let _ = Lexer::new_from_str(txt).scan_tokens();
    }
    #[test]
    #[should_panic]
    fn num_2() {
        let txt = "0o99";
        let _ = Lexer::new_from_str(txt).scan_tokens();
    }

    #[test]
    #[should_panic]
    fn num_3() {
        let txt = "0.";
        let t = Lexer::new_from_str(txt).scan_tokens();
        assert_ne!(t.len() - 1, 1);
    }

    #[test]
    fn num_4() {
        use PunctuatorKind::*;
        use TokenKind::*;
        let txt = ".0";
        let t = Lexer::new_from_str(txt).scan_tokens();
        assert_eq!(t.len() - 1, 2);
        assert_eq!(t[0].kind, Punctuator(Dot));
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
            Literal(Integer {
                content: "2222".to_owned(),
                base: None,
                suffix: None
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
