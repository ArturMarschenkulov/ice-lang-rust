use crate::error::*;
use crate::token;
use crate::token::*;
use unicode_xid::UnicodeXID;

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
        let mut punc_vec = Vec::new();
        let mut tokens_pass_0 = Vec::new();
        while let Some(ch) = self.peek(0) {
            let token = self.scan_token(ch);
            //println!("oswlo: {:?}", token);

            if token.kind.can_be_part_of_complex() {
                punc_vec.push(token);
            } else {
                if !punc_vec.is_empty() {
                    let punc_token = conv_to_complex(&punc_vec);
                    punc_vec.clear();
                    tokens_pass_0.push(punc_token);
                }
                if !token.kind.is_to_skip() {
                    tokens_pass_0.push(token.clone());
                }
            }
        }

        use SpecialKeywordKind::*;
        use TokenKind::*;
        let token_eof = Token::new(
            SpecialKeyword(Eof),
            Span::from_tuples((1, 1), (1, 1)),
            token::Whitespace::None,
        );
        tokens_pass_0.push(token_eof);
        tokens_pass_0
    }

    fn scan_token_kind(&mut self, c: char) -> Result<TokenKind, LexerError> {
        use PunctuatorKind::*;
        use SpecialKeywordKind::*;
        use TokenKind::*;

        let kind = match c {
            '+' => Ok(Punctuator(Plus)),
            '-' => Ok(Punctuator(Minus)),
            '*' => Ok(Punctuator(Asterisk)),
            '=' => Ok(Punctuator(Equal)),
            '!' => Ok(Punctuator(Exclamation)),
            '>' => Ok(Punctuator(Greater)),
            '<' => Ok(Punctuator(Less)),
            '&' => Ok(Punctuator(Ampersand)),
            '|' => Ok(Punctuator(VerticalBar)),
            '%' => Ok(Punctuator(Percent)),
            '$' => Ok(Punctuator(Dollar)),
            '?' => Ok(Punctuator(Question)),
            ',' => Ok(Punctuator(Comma)),
            '#' => Ok(Punctuator(Hash)),
            '\\' => Ok(Punctuator(Backslash)),
            '@' => Ok(Punctuator(At)),

            '(' => Ok(Punctuator(LeftParen)),
            ')' => Ok(Punctuator(RightParen)),
            '[' => Ok(Punctuator(LeftBracket)),
            ']' => Ok(Punctuator(RightBracket)),
            '{' => Ok(Punctuator(LeftBrace)),
            '}' => Ok(Punctuator(RightBrace)),

            ':' => Ok(Punctuator(Colon)),
            ';' => Ok(Punctuator(Semicolon)),
            ' ' | '\r' | '\t' => Ok(SpecialKeyword(Whitespace)),
            '\n' => Ok(SpecialKeyword(Newline)),

            '/' => match self.peek_into_str(1).unwrap().as_str() {
                "//" => self.lex_comment_line(),
                "/*" => self.lex_comment_block(),
                _ => Ok(Punctuator(Slash)),
            },
            '.' => match self.peek(1) {
                Some(s) if is_digit(s) && !self.check_char_at('.', -1).ok().is_some() => {
                    self.lex_number()
                }
                _ => Ok(Punctuator(Dot)),
            },
            '"' => self.lex_string(),
            '\'' => self.lex_char(),
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

        let token_start_index = 0 - (end_index - start_index) as isize;
        let token_end_index = -1 as isize;

        let tok_start_left = self.peek(token_start_index - 1);
        let tok_end_right = self.peek(token_end_index + 1);

        let is_ws_left = token::Whitespace::is_left_whitespace(tok_start_left.unwrap_or(' '));
        let is_ws_right = token::Whitespace::is_right_whitespace(tok_end_right.unwrap_or(' '));

        let whitespace = token::Whitespace::from((is_ws_left, is_ws_right));

        Token::new(token_kind, Span::new(start_pos, end_pos), whitespace)
    }
    fn lex_escape_char(&mut self) -> Result<char, LexerError> {
        self.eat_char('\\').unwrap();
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
        self.eat_char('\'').unwrap();

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

        match self.eat_char('\'') {
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
                    && !self.check_char_at('.', 1).ok().is_some() =>
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
        use TokenKind::*;
        use LiteralKind::*;

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
            _ => panic!("Identifier not a keyword or identifier."),
        };
        Ok(token)
    }
}

/// This impl is the cursor part for the lexer.
///
impl Lexer {
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
        let new_offset = self.calc_offset(offset);
        match new_offset {
            Some(new_offset) => self.chars.get(new_offset).copied(),
            None => None,
        }
    }
    fn is_inbounds(&self, offset: isize) -> bool {
        let new_offset = self.calc_offset(offset);
        match new_offset {
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
    fn check_char_at(&self, c: char, n: isize) -> Result<char, Option<char>> {
        match self.peek(n) {
            Some(ch) if c == ch => Ok(c),
            Some(other) => Err(Some(other)),
            None => Err(None),
        }
    }
    fn check_char(&self, c: char) -> Result<char, Option<char>> {
        self.check_char_at(c, 0)
    }
    fn check_str(&self, s: &str) -> Option<String> {
        if self.peek_into_str(s.len() - 1) == Some(s.to_string()) {
            Some(s.to_string())
        } else {
            None
        }
    }
    /// Matches a terminal character. If the character is matched, an Option with that character is returned, otherwise None.
    fn eat_char(&mut self, ch: char) -> Option<char> {
        let c = self.check_char(ch);
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
            if let Some(c) = self.eat_char(*ch) {
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
