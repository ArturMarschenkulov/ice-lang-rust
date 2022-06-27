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
    Lexer::new().scan_tokens(source)
}

struct Lexer {
    // chars_: Chars<'a>,
    chars: Vec<char>,
    index: usize,
    cursor: Position,
}

impl Lexer {
    fn new() -> Self {
        Self {
            chars: "".chars().collect(),
            index: 0,
            cursor: Position { line: 1, column: 1 },
        }
    }
    fn from_string(string: String) -> Self {
        todo!()
    }
    fn scan_tokens(&mut self, source: &str) -> Vec<Token> {
        self.chars = source.chars().collect();

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
        tokens_pass_2.push(Token {
            kind: SpecialKeyword(Eof),
            span: Span {
                start: Position { line: 0, column: 0 },
                end: Position { line: 0, column: 0 },
            },
            whitespace: token::Whitespace::Undefined,
        });
        tokens_pass_2
    }

    fn scan_token_kind(&mut self, c: char) -> TokenKind {
        use PunctuatorKind::*;
        use SpecialKeywordKind::*;
        use TokenKind::*;
        let kind = match c {
            '+' => Punctuator(Plus),
            '-' => Punctuator(Minus),
            '*' => Punctuator(Star),
            '/' => {
                let next_char = self.peek(1).unwrap();
                if next_char == '/' {
                    self.lex_comment_line()
                } else if next_char == '*' {
                    self.lex_comment_block()
                } else {
                    Punctuator(Slash)
                }
            }
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
                Some(s) if is_digit(s) => self.lex_number(),
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
            '"' => self.lex_string(),
            cc if is_digit(cc) => self.lex_number(),
            cc if is_alpha(cc) => self.lex_identifier(),
            cc => match cc {
                // cc => {
                //     UnicodeXID::is_xid_start(cc);
                //     todo!()
                // }
                _ => panic!(
                    "Character '{}' is unknown. At '{}:{}'.",
                    cc, self.cursor.line, self.cursor.column
                ),
            },
        };
        // NOTE: If the token is simple we advance here, because the match statements look prettier that way.
        // With more complex tokens, basically everything except single punctuators,
        // handle the advance in their own functions
        if kind.is_simple() {
            self.advance();
        } else {
        }
        kind
    }

    /// Scans a token
    /// Returns the token which was lexed. It also moves the 'cursor' to the next character, so that one can lex the next token.
    fn scan_token(&mut self, c: char) -> Token {
        use SpecialKeywordKind::*;
        let start_pos = self.cursor;

        let token_kind = self.scan_token_kind(c);

        let end_pos = self.cursor;
        if token_kind == TokenKind::SpecialKeyword(Newline) {
            self.cursor.line += 1;
            self.cursor.column = 1;
        } else {
            self.cursor.column += 1;
        }
        Token {
            kind: token_kind,
            span: Span {
                start: start_pos,
                end: end_pos,
            },
            whitespace: token::Whitespace::Undefined,
        }
    }
    fn lex_escape_char(&mut self) -> String {
        self.match_char('\\').unwrap();
        self.cursor.column += 1;

        let mut ch = String::new();
        let escaped_char = match self.peek(0).unwrap() {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\',
            '\'' => '\'',
            '"' => '"',
            '0' => '\0',
            _ => panic!("Unknown escape sequence"),
        };
        self.advance();
        self.cursor.column += 1;

        ch.push(escaped_char);
        ch
    }
    fn lex_string(&mut self) -> TokenKind {
        self.match_char('\"').unwrap();

        let mut string_content = String::new();
        while let Some(c) = self.peek(0) {
            if c == '"' {
                break;
            }
            // self.advance();
            // handle escape characters

            match c {
                '\\' => {
                    let escape_char = self.lex_escape_char();
                    string_content.push_str(&escape_char);
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
        self.match_char('\"').unwrap();
        self.cursor.column += (2 - 1) as u32;

        if self.peek(0) == None {
            panic!("Unterminated string.");
        }
        TokenKind::Literal(LiteralKind::String(string_content))
    }
    fn lex_comment_line(&mut self) -> TokenKind {
        // TODO: Implement that a comment at the end of a file is possible, meanign when it does not end through a newline, but eof
        self.match_char('/').unwrap();
        self.match_char('/').unwrap();

        while self.peek(0) != Some('\n') && self.peek(0) != None {
            self.advance();
            self.cursor.column += 1;
        }
        //assert!(self.peek(0) == Some('\n'));
        TokenKind::SpecialKeyword(SpecialKeywordKind::Comment)
    }
    fn lex_comment_block(&mut self) -> TokenKind {
        // TODO: Implement nested block comments
        self.match_char('/').unwrap();
        self.cursor.column += 1;
        self.match_char('*').unwrap();
        self.cursor.column += 1;

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

        self.match_char('*').unwrap();
        self.cursor.column += 1;
        self.match_char('/').unwrap();
        self.cursor.column += 1;
        TokenKind::SpecialKeyword(SpecialKeywordKind::Comment)
    }
    fn lex_number(&mut self) -> TokenKind {
        use LiteralKind::*;
        use TokenKind::*;

        let mut has_base_prefix = true;
        let mut number_base = NumberBase::Decimal;
        match self.peek_str(2).unwrap().as_str() {
            "0b" => number_base = NumberBase::Binary,
            "0o" => number_base = NumberBase::Octal,
            "0d" => number_base = NumberBase::Decimal,
            "0x" => number_base = NumberBase::Hexadecimal,
            _ => has_base_prefix = false,
        }

        let is_in_number_base = match number_base {
            NumberBase::Binary => is_binary,
            NumberBase::Octal => is_octal,
            NumberBase::Decimal => is_digit,
            NumberBase::Hexadecimal => is_hexadecimal,
        };
        if has_base_prefix {
            self.advance();
            self.cursor.column += 1;
            self.advance();
            self.cursor.column += 1;
        }
        let mut is_after_dot = false;
        let mut is_floating = false;
        let mut string = std::string::String::new();
        while let Some(c) = self.peek(0) {
            if is_in_number_base(c) {
                string.push(c);
                self.advance();
                if is_after_dot {
                    is_after_dot = false;
                }
            } else if c == '.' && !is_floating && !has_base_prefix {
                string.push(c);
                self.advance();
                is_floating = true;
                is_after_dot = true;
            } else if c == '_' && !is_after_dot {
                string.push(c);
                self.advance();
            } else {
                break;
            }
        }

        let _starts_with_dot = string.starts_with('.');
        let _ends_with_dot = string.ends_with('.');
        println!("{}", string.len());
        self.cursor.column += (string.len() - 1) as u32;

        if is_floating {
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
        }
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
impl Lexer {
    fn is_inbounds(&self, offset: usize) -> bool {
        self.index + offset <= self.chars.len()
    }
    fn peek(&self, n: usize) -> Option<char> {
        if self.is_inbounds(n) {
            self.chars.get(self.index + n).copied()
        } else {
            None
        }
    }
    /// peeks next `n` consecutive chars and returns them as a string.
    fn peek_str(&self, n: usize) -> Option<String> {
        if self.is_inbounds(n) {
            let mut str = String::new();
            for i in 0..n {
                let c = self.chars.get(self.index + i).copied().unwrap();
                str.push(c);
            }
            return Some(str);
        } else {
            None
        }
    }

    // fn eat_char(&mut self) -> Option<char> {
    //     let c = self.chars[self.index];
    //     self.index += 1;
    //     c
    // }
    fn advance(&mut self) {
        self.index += 1;
    }

    /// Matches a terminal character. If the character is matched, an Option with that character is returned, otherwise None.
    fn match_char(&mut self, ch: char) -> Option<char> {
        if self.peek(0) == Some(ch) {
            self.advance();
            Some(ch)
        } else {
            None
        }
    }
    fn match_chars(&mut self, chars: &[char]) -> Option<char> {
        for ch in chars {
            if self.peek(0) == Some(*ch) {
                self.advance();
                return Some(*ch);
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

        let mut lexer = Lexer::new();
        let tokens = lexer.scan_tokens(&source.to_owned());

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
