use crate::token::*;
use std::str::Chars;


fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}
fn is_alpha(c: char) -> bool {
	(c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}
fn is_alpha_numeric(c: char) -> bool {
    is_digit(c) || is_alpha(c)
}
fn is_skip_token(token: &Token) -> bool {
    use TokenKind::*;
    use SpecialKeywordKind::*;
    match token.kind {
        SpecialKeyword(Whitespace) | SpecialKeyword(Newline) | SpecialKeyword(Comment) => true,
        _ => false,
    }
}
#[allow(dead_code)]
struct Cursor<'a> {
    chars: Chars<'a>,
}
#[allow(dead_code)]
impl<'a> Cursor<'a> {
    fn new(&mut self, _string: String) -> Self {
        unimplemented!()
    }
    fn chars(&self) -> Chars<'a> {
        self.chars.clone()
    }
    fn advance(&mut self) {
        self.chars.next();
    }
    fn peek(&self, n: usize) -> char {
        self.chars().clone().nth(n).unwrap()
    }
}
pub fn get_tokens_from_source(source: &str) -> Vec<Token> {
    Tokenizer::new().scan_tokens(source)
}
struct Tokenizer<'a> {
    chars: Chars<'a>
}

impl<'a> Tokenizer<'a> {
    fn new() -> Self {
        Self {
            chars: "".chars(),
        }
    }
    fn scan_tokens(&mut self, source: &'a str) -> Vec<Token> {
        self.chars = source.chars();
        let mut tokens= Vec::new();

        while let Some(ch) = self.peek(0) {
            let token = self.scan_token(ch);

            if !is_skip_token(&token) {
                tokens.push(token);
            }
            self.advance();
        }
        tokens.push(Token { kind: TokenKind::SpecialKeyword(SpecialKeywordKind::Eof), });
        tokens
    }
    fn scan_token(&mut self, c: char) -> Token {
        let token_kind = self.determine_token_type(c);
        self.create_token_from_type(token_kind)
    }

    fn create_token_from_type(&mut self, kind: TokenKind) -> Token {
        match kind {
            _ => {
                Token { kind }
            }
        }
    }

    fn determine_token_type(&mut self, c: char) -> TokenKind {
        use TokenKind::*;
        use PunctuatorKind::*;
        use SpecialKeywordKind::*;

        match c  {
            '+' => {
                if self.peek(1) == Some('=') {
                    self.advance();
                    Punctuator(PlusEqual)
                } else {
                    Punctuator(Plus)
                }
            },
            '-' => {
                if self.peek(1) == Some('=') {
                    self.advance();
                    Punctuator(MinusEqual)
                }else if self.peek(1) == Some('>') {
                    self.advance();
                    Punctuator(MinusGreater)
                } else {
                    Punctuator(Minus)
                }

            },
            '*' => {
                if self.peek(1) == Some('=') {
                    self.advance();
                    Punctuator(StarEqual)
                } else {
                    Punctuator(Star)
                }
            },
            '/' => {
                if self.peek(1) == Some('=') {
                    self.advance();
                    Punctuator(SlashEqual)
                } else if self.peek(1) == Some('/') {
                    self.lex_comment_line()
                } else if self.peek(1) == Some('*') {
                    self.lex_comment_block()
                } else {
                    Punctuator(Slash)
                }
            },
            '=' => {
                if self.peek(1) == Some('=') {
                    self.advance();
                    Punctuator(EqualEqual)
                } else {
                    Punctuator(Equal)
                }
            },
            '!' => {
                if self.peek(1) == Some('=') {
                    self.advance();
                    Punctuator(BangEqual)
                } else {
                    Punctuator(Bang)
                }
            },
            '>' => {
                if self.peek(1) == Some('=') {
                    self.advance();
                    Punctuator(GreaterEqual)
                } else {
                    Punctuator(Greater)
                }
            },
            '<' => {
                if self.peek(1) == Some('=') {
                    self.advance();
                    Punctuator(LessEqual)
                } else {
                    Punctuator(Less)
                }
            },
            '&' => {
                if self.peek(1) == Some('&') {
                    self.advance();
                    Punctuator(AmpersandAmpersand)
                } else {
                    Punctuator(Ampersand)
                }
            },
            '|' => {
                if self.peek(1) == Some('|') {
                    self.advance();
                    Punctuator(PipePipe)
                } else {
                    Punctuator(Pipe)
                }
            },

            '%' => Punctuator(Percent),
            '$' => Punctuator(Dollar),
            '?' => Punctuator(Question),
            ',' => Punctuator(Comma),
            '.' => Punctuator(Dot),
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
            '"' => {
                self.lex_string()
            },
            cc if is_digit(cc) => self.lex_number(),
            cc if is_alpha(cc) => self.lex_identifier(),
            cc => panic!("This is an unknown character {:?}.", cc),
        }
    }
    fn lex_string(&mut self) -> TokenKind {
        let mut string_content = String::new();
        while let Some(c) = self.peek(1) {
            if c != '"' {
                // handle escape characters
                if self.peek(1) == Some('\\') {
                    let mut is_escape = false;
                    match self.peek(2) {
                        Some('n') => {
                            is_escape = true;
                            string_content.push('\n');
                        },
                        Some('t') => {
                            is_escape = true;
                            string_content.push('\t');
                        },
                        Some('0') => {
                            is_escape = true;
                            string_content.push('\0');
                        },
                        Some('\\') => {
                            is_escape = true;
                            string_content.push('\\');
                        },
                        Some('\"') => {
                            is_escape = true;
                            string_content.push('\"');
                        },
                        Some('\'') => {
                            is_escape = true;
                            string_content.push('\'');
                        },
                        _ => {

                        },
                    }
                    if is_escape {
                        self.advance();
                        self.advance();
                    }
                } else if self.peek(1) == Some('\n') || self.peek(1) == Some('\t') {
                    self.advance();
                } else  {
                    string_content.push(c);
                    self.advance();
                }
            } else {
                break
            }
        };
        self.advance();
        if self.peek(0) == None {
            panic!("Unterminated string.");
        }
        TokenKind::Literal(LiteralKind::String(string_content))
    }
    fn lex_comment_line(&mut self) -> TokenKind {
        self.advance();
        while self.peek(1) != Some('\n') && self.peek(1) != Some('\0') {
            self.advance();
        }
        TokenKind::SpecialKeyword(SpecialKeywordKind::Comment)
    }
    fn lex_comment_block(&mut self) -> TokenKind {
        self.advance();
        while self.peek(1) != Some('*') && self.peek(2) != Some('/') {
            self.advance();
        }
        self.advance();
        self.advance();
        TokenKind::SpecialKeyword(SpecialKeywordKind::Comment)
    }
    fn lex_number(&mut self) -> TokenKind {
        let mut string_content = String::new();
        string_content.push(self.peek(0).unwrap());
        while let Some(c) = self.peek(1) {
            if is_digit(c) {
                string_content.push(c);
                self.advance();
            } else {
                break
            }
        }
        //let ss = String::from("66");
        TokenKind::Literal(LiteralKind::Integer(string_content.parse::<i64>().unwrap()))
    }
    fn lex_identifier(&mut self) -> TokenKind {
        let mut string_content = String::new();
        string_content.push(self.peek(0).unwrap());
        while let Some(c) = self.peek(1) {
            if is_alpha_numeric(c) {
                string_content.push(c);
                self.advance();
            } else {
                break
            }
        }

        //determine keywords
        match string_content.as_ref() {
            "var" => TokenKind::Keyword(KeywordKind::Var),
            "fn" => TokenKind::Keyword(KeywordKind::Fn),
            "if" => TokenKind::Keyword(KeywordKind::If),
            "else" => TokenKind::Keyword(KeywordKind::Else),
            "while" => TokenKind::Keyword(KeywordKind::While),
            "for" => TokenKind::Keyword(KeywordKind::For),

            "true" => TokenKind::Keyword(KeywordKind::True),
            "false" => TokenKind::Keyword(KeywordKind::False),


            "print" => TokenKind::Keyword(KeywordKind::Print),
            "println" => TokenKind::Keyword(KeywordKind::Println),
            _ => TokenKind::Identifier(string_content),
        }

    }

    fn peek(&mut self, n: usize) -> Option<char> {
        if let Some(ch) = &self.chars.clone().nth(n) {
            Some(*ch)
        } else {
            None
        }
    }
    fn advance(&mut self) {
        self.chars.next();
    }
}
#[cfg(test)]
mod test {
    use crate::tokenizer::Tokenizer;
    use crate::token::{TokenKind, KeywordKind, PunctuatorKind, LiteralKind};

    #[test]
    fn test_0() {
        let source = "var a = 2222;";

        let token = Tokenizer::new().scan_tokens(source);

        assert_eq!(token[0].kind, TokenKind::Keyword(KeywordKind::Var));
        assert_eq!(token[1].kind, TokenKind::Identifier(String::from("a")));
        assert_eq!(token[2].kind, TokenKind::Punctuator(PunctuatorKind::Equal));
        assert_eq!(token[3].kind, TokenKind::Literal(LiteralKind::Integer(2222)));
        assert_eq!(token[4].kind, TokenKind::Punctuator(PunctuatorKind::Semicolon));
    }
}