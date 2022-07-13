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
        // self.chars = source.chars().collect();

        // The idea is to have several passes. The first pass basically gets the raw tokens.
        // Meaning there are no composite tokens (e.g. ==), whitespaces and comments are included, etc.
        // The second pass glues together the tokens into composite tokens and handles comments etc.
        // The third pass handles whitespaces and comments.
        // Of course, one can do the same thing in only one pass, which would be more effificient,
        // but this way the structure is easier to modify. This might help in implementing custom operators.

        let mut tokens_pass_0 = Vec::new();
        while let Some(ch) = self.peek(0) {
            let token = self.scan_token(ch);
            println!("{:?}", token);
            tokens_pass_0.push(token);
        }
        let tokens_pass_0 = tokens_pass_0;

        // Second pass: We glue together raw tokens into composite tokens.

        let mut tokens_pass_1 = Vec::new();

        {
            let mut punc_vec = Vec::new();

            let mut prev_token_kind: Option<TokenKind> = None;
            let mut tok_iter = tokens_pass_0.iter();
            while let Some(token) = tok_iter.next() {
                let mut token = token.clone();

                let next_token_kind = tok_iter.clone().next().map(|tok| tok.kind.clone());

                token.whitespace = token::Whitespace::from_token_kinds(
                    prev_token_kind.as_ref(),
                    next_token_kind.as_ref(),
                );

                prev_token_kind = Some(token.kind.clone());

                if token.kind.can_be_part_of_complex() {
                    punc_vec.push(token);
                } else {
                    if !punc_vec.is_empty() {
                        let punc_token = conv_to_complex(&punc_vec);
                        punc_vec.clear();
                        tokens_pass_1.push(punc_token);
                    }

                    tokens_pass_1.push(token.clone());
                }
            }
        }
        let tokens_pass_1 = tokens_pass_1;

        // Third pass. We handle whitespaces and comments.
        let mut tokens_pass_2: Vec<Token> = Vec::new();
        {
            for token in tokens_pass_1.iter() {
                if !token.kind.is_to_skip() {
                    tokens_pass_2.push(token.clone());
                }
            }
        }

        for tp in &tokens_pass_2 {
            if tp.whitespace == token::Whitespace::Undefined {
                panic!("Token has no whitespace: {:?}", tp);
            }
        }
        use SpecialKeywordKind::*;
        use TokenKind::*;
        let token_eof = Token::new_undefind_whitespace(
            SpecialKeyword(Eof),
            Span::from_tuples((1, 1), (1, 1)),
        );
        tokens_pass_2.push(token_eof);
        tokens_pass_2
    }

    fn scan_token_kind(&mut self, c: char) -> Result<TokenKind, LexerError> {
        use PunctuatorKind::*;
        use SpecialKeywordKind::*;
        use TokenKind::*;
        let kind = match c {
            '+' => Punctuator(Plus),
            '-' => Punctuator(Minus),
            '*' => Punctuator(Star),
            '/' => match self.peek_into_str(1).unwrap().as_str() {
                "//" => self.lex_comment_line(),
                "/*" => self.lex_comment_block(),
                _ => Punctuator(Slash),
            },
            '=' => Punctuator(Equal),
            '!' => Punctuator(Bang),
            '>' => Punctuator(Greater),
            '<' => Punctuator(Less),
            '&' => Punctuator(Ampersand),
            '|' => Punctuator(Pipe),
            '%' => Punctuator(Percent),
            '$' => Punctuator(Dollar),
            '?' => Punctuator(Question),
            ',' => Punctuator(Comma),
            '.' => match self.peek(1) {
                Some(s) if is_digit(s) && !self.match_char_at('.', -1).is_some() => self.lex_number().unwrap(),
                _ => Punctuator(Dot),
            },
            '#' => Punctuator(Hash),
            '\\' => Punctuator(Backslash),
            '@' => Punctuator(At),
            '(' => Punctuator(LeftParen),
            ')' => Punctuator(RightParen),
            '[' => Punctuator(LeftBracket),
            ']' => Punctuator(RightBracket),
            '{' => Punctuator(LeftBrace),
            '}' => Punctuator(RightBrace),

            ':' => Punctuator(Colon),
            ';' => Punctuator(Semicolon),

            ' ' | '\r' | '\t' => SpecialKeyword(Whitespace),
            '\n' => SpecialKeyword(Newline),
            '"' => self.lex_string().unwrap(),
            '\'' => self.lex_char().unwrap(),
            cc if is_digit(cc) => self.lex_number().unwrap(),
            cc if is_alpha(cc) => self.lex_identifier(),
            cc => match cc {
                // cc => {
                //     UnicodeXID::is_xid_start(cc);
                //     todo!()
                // }
                _ => {
                    return Err(LexerError::new(format!(
                        "Character '{}' is unknown. At '{}:{}'.",
                        cc, self.cursor.line, self.cursor.column
                    )))
                }
            },
        };
        // NOTE: If the token is simple we advance here, because the match statements look prettier that way.
        // With more complex tokens, basically everything except single punctuators,
        // handle the advance in their own functions
        if kind.is_simple() {
            self.advance();
        } else {
        }
        Ok(kind)
    }

    /// Scans a token
    /// Returns the token which was lexed. It also moves the 'cursor' to the next character, so that one can lex the next token.
    fn scan_token(&mut self, c: char) -> Token {
        use SpecialKeywordKind::*;
        let start_pos = self.cursor;

        let token_kind = self.scan_token_kind(c).unwrap();

        let end_pos = self.cursor;
        if token_kind == TokenKind::SpecialKeyword(Newline) {
            self.cursor.line += 1;
            self.cursor.column = 1;
        } else {
            self.cursor.column += 1;
        }
        Token::new_undefind_whitespace(token_kind, Span::new(start_pos, end_pos))
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

        escaped_char
    }
    fn lex_char(&mut self) -> Result<TokenKind, LexerError> {
        use LiteralKind::*;
        use TokenKind::*;
        self.eat_char('\'').unwrap();

        let ch = if let Some(c) = self.peek(0) {
            let ch = if c == '\'' {
                return Err(LexerError::new(format!("empty character literal")));
            } else if c == '\\' {
                self.lex_escape_char()?
            } else if c == '\n' || c == '\r' {
                return Err(LexerError::new(format!("newline in character literal")));
            } else {
                self.advance();
                self.cursor.column += 1;
                c
            };
            Some(ch)
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
        Ok(Literal(Char(ch.unwrap())))
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
    fn lex_comment_line(&mut self) -> TokenKind {
        // TODO: Implement that a comment at the end of a file is possible, meanign when it does not end through a newline, but eof
        self.eat_str("////").unwrap();

        while self.peek(0) != Some('\n') && self.peek(0) != None {
            self.advance();
            self.cursor.column += 1;
        }
        //assert!(self.peek(0) == Some('\n'));
        TokenKind::SpecialKeyword(SpecialKeywordKind::Comment)
    }
    fn lex_comment_block(&mut self) -> TokenKind {
        // TODO: Implement nested block comments
        self.eat_str("/*").unwrap();
        self.cursor.column += 2; 

        while self.peek(0) != Some('*') && self.peek(1) != Some('/') {
            self.advance();

            if self.peek(0) == Some('\n') {
                self.cursor.line += 1;
                self.cursor.column = 1;
            } else {
                self.cursor.column += 1;
            }
            self.cursor.column += 1;
        }
        
        self.eat_str("*/").unwrap();
        self.cursor.column += 2;
        TokenKind::SpecialKeyword(SpecialKeywordKind::Comment)
    }
    fn lex_number(&mut self) -> Result<TokenKind, LexerError> {
        use LiteralKind::*;
        use TokenKind::*;

        let (number_base, has_base_prefix, is_in_number_base)
            : (NumberBase, bool, fn(char) -> bool) 
            = match self.peek_into_str(1).unwrap().as_str() {
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
        let mut string = std::string::String::new();
        while let Some(c) = self.peek(0) {
            match c {
                '.' if is_floating => {
                    return Err(LexerError::new(format!("multiple dots in number")));
                }
                '.' if has_base_prefix => {
                    return Err(LexerError::new(format!("dot after base prefix")));
                }
                // in a number there can be only one dot
                // it can't be in a number which has a prefix, because floating points don't have a base prefix
                '.' if !is_floating && !has_base_prefix && self.peek(1).unwrap() != '.'  => {
                    //if self.peek(1).unwrap() != '.' {
                        string.push(c);
                        self.advance();
                        is_floating = true;
                        is_after_dot = true;
                    //}
                }
                // a '_' is not possible after a '.'. E.g. '1._3' is not a number, this is an integer with a member variable.
                '_' if !is_after_dot => {
                    //string.push(c);
                    self.advance();
                }
                // checks whether it is a possible digit at all.
                c if is_hexadecimal(c) => {
                    if is_in_number_base(c) {
                        string.push(c);
                        self.advance();
                        if is_after_dot {
                            is_after_dot = false;
                        }
                    } else {
                        return Err(LexerError::new(format!(
                            "invalid digit for base {} literal",
                            number_base.as_num()
                        )));
                    }
                }
                _ => break,
            };
        }

        let _starts_with_dot = string.starts_with('.');
        let _ends_with_dot = string.ends_with('.');
        self.cursor.column += (string.len() - 1) as u32;

        let lit = if is_floating {
            //let error_msg = format!("Could not parse floating point number: {}", string);
            //string.parse::<f64>().expect(&error_msg);

            Literal(Floating { content: string })
        } else {
            //let error_msg = format!("Could not parse integer number: {}", string);
            //string.parse::<u128>().expect(&error_msg);
            Literal(Integer {
                content: string,
                base: number_base,
            })
        };
        Ok(lit)
    }
    fn lex_identifier(&mut self) -> TokenKind {
        let mut string_content = String::new();
        while let Some(ch) = self.peek(0) {
            if is_alpha_numeric(ch) {
                string_content.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        //determine keywords
        use KeywordKind::*;
        use TokenKind::*;
        let token = match string_content.as_ref() {
            "var" => Keyword(Var),
            "fn" => Keyword(Fn),
            "if" => Keyword(If),
            "else" => Keyword(Else),
            "while" => Keyword(While),
            "for" => Keyword(For),

            "true" => Keyword(True),
            "false" => Keyword(False),

            "print" => Keyword(Print),
            "println" => Keyword(Println),
            _ => Identifier(string_content),
        };
        match &token {
            Keyword(kw) => self.cursor.column += (kw.get_length() - 1) as u32,
            Identifier(id) => self.cursor.column += (id.len() - 1) as u32,
            _ => panic!("Identifier not a keyword or identifier."),
        };
        token
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

    fn peek(&self, n: isize) -> Option<char> {
        let new_offset = if n.is_negative() {
            self.index.checked_sub(-n as usize)
        } else if n.is_positive(){
            self.index.checked_add(n as usize)
        } else {
            Some(self.index)
        };
        match new_offset {
            Some(new_offset) => self.chars.get(new_offset).copied(),
            None => None,
        }
    }
    fn is_inbounds(&self, offset: isize) -> bool {
        let new_offset = if offset.is_negative() {
            self.index.checked_sub(-offset as usize)
        } else if offset.is_positive(){
            self.index.checked_add(offset as usize)
        } else {
            Some(self.index)
        };
        match new_offset {
            Some(new_offset) => new_offset < self.chars.len(),
            None => false,
        }
    }
    /// peeks up to `n` consecutive chars and returns them as a string. If 'n == 0', refers to the current
    fn peek_into_str(&self, n: isize) -> Option<String> {
        if self.is_inbounds(n) {
            let mut str = String::new();
            for i in 0..=n {
                let c = self.peek(i).unwrap();
                str.push(c);
            }
            Some(str)
        } else {
            None
        }
    }

    /// peeks at the offset `n`. If the peeked char is the same as `c`, it returns it wrapped in a `Some`, otherwise `None`.
    fn match_char_at(&self, c: char, n: isize) -> Option<char>  {
        if self.peek(n) == Some(c) {
            Some(c)
        } else {
            None
        }
    }
    fn match_char(&self, c: char) -> Option<char> {
        self.match_char_at(c, 0)
    }
    /// Matches a terminal character. If the character is matched, an Option with that character is returned, otherwise None.
    fn eat_char(&mut self, ch: char) -> Option<char> {
        let c = self.match_char(ch);
        if c.is_some() {
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
            for n in 0..chars.len() {
                self.advance();
            }
            Some(result)
        } else {
            panic!();
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
