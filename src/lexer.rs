use crate::token;
use crate::token::*;
use std::str::Chars;
fn is_digit(c: char) -> bool {
    ('0'..='9').contains(&c)
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
struct Cursor<'a> {
    chars: Chars<'a>,
}
#[allow(dead_code)]
impl<'a> Cursor<'a> {
    fn new(_string: String) -> Self {
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
    Lexer::new().scan_tokens(source)
}
struct Lexer<'a> {
    chars: Chars<'a>,
    position: Position,
}

impl<'a> Lexer<'a> {
    fn new() -> Self {
        Self {
            chars: "".chars(),
            position: Position { line: 1, column: 1 },
        }
    }
    fn scan_tokens(&mut self, source: &'a str) -> Vec<Token> {
        self.chars = source.chars();

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

                let next_token_kind = match tok_iter.clone().next() {
                    Some(tok) => Some(tok.kind.clone()),
                    None => None,
                };

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
            cc => panic!("This is an unknown character {:?}.", cc),
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
        let start_pos = self.position;

        let token_kind = self.scan_token_kind(c);

        let end_pos = self.position;
        if token_kind == TokenKind::SpecialKeyword(Newline) {
            self.position.line += 1;
            self.position.column = 1;
        } else {
            self.position.column += 1;
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
        let mut ch = String::new();
        if self.peek(1) == Some('\\') {
            let mut is_escape = true;
            match self.peek(2) {
                Some('n') => ch.push('\n'),
                Some('t') => ch.push('\t'),
                Some('0') => ch.push('\0'),
                Some('\\') => ch.push('\\'),
                Some('\"') => ch.push('\"'),
                Some('\'') => ch.push('\''),
                _ => is_escape = false,
            }
            if is_escape {
                self.advance();
                self.advance();
                self.position.column += 2;
            }
        }
        ch
    }
    fn lex_string(&mut self) -> TokenKind {
        // TODO: Are the delimiter of a string, part of its span? Right now they are. Think about it!
        assert!(self.peek(0) == Some('\"'));
        self.advance();

        let mut string_content = String::new();
        while let Some(c) = self.peek(0) {
            if c == '"' {
                break;
            }
            // handle escape characters
            if self.peek(1) == Some('\\') {
                let escape_char = self.lex_escape_char();
                string_content.push_str(&escape_char);
            } else if self.peek(1) == Some('\n') {
                self.position.column = 1;
                self.position.line += 1;
                self.advance();
            } else if self.peek(1) == Some('\t') {
                // TODO: Figure out how to correctly handle a tab, because they can be variable length.
                self.position.column += 1;
                self.advance();
            } else {
                string_content.push(c);
                self.position.column += 1;
                self.advance();
            }
        }
        assert!(self.peek(0) == Some('\"'));
        self.advance(); // this is for the '"'
        self.position.column += (2 - 1) as u32;

        if self.peek(0) == None {
            panic!("Unterminated string.");
        }
        TokenKind::Literal(LiteralKind::String(string_content))
    }
    fn lex_comment_line(&mut self) -> TokenKind {
        assert!(self.peek(0) == Some('/'));
        self.advance();
        assert!(self.peek(0) == Some('/'));
        self.advance();

        while self.peek(0) != Some('\n') && self.peek(0) != Some('\0') {
            self.advance();
            self.position.column += 1;
        }
        TokenKind::SpecialKeyword(SpecialKeywordKind::Comment)
    }
    fn lex_comment_block(&mut self) -> TokenKind {
        // TODO: Implement nested block comments
        assert!(self.peek(0) == Some('/'));
        self.advance(); // for '/'
        assert!(self.peek(0) == Some('*'));
        self.advance(); // for '*'
        self.position.column += 2;

        while self.peek(0) != Some('*') && self.peek(1) != Some('/') {
            self.advance();

            if self.peek(1) == Some('\n') {
                self.position.line += 1;
                self.position.column = 1;
            } else {
                self.position.column += 1;
            }
            self.position.column += 1;
        }

        assert!(self.peek(0) == Some('*'));
        self.advance(); // for '*'
        assert!(self.peek(0) == Some('/'));
        self.advance(); // for '/'

        self.position.column += 2;
        TokenKind::SpecialKeyword(SpecialKeywordKind::Comment)
    }
    fn lex_number(&mut self) -> TokenKind {
        // use LiteralKind::*;

        let mut is_floating = false;
        let mut is_after_dot = false;

        let mut string = String::new();
        while let Some(c) = self.peek(0) {
            if is_digit(c) {
                string.push(c);
                self.advance();
                if is_after_dot {
                    is_after_dot = false;
                }
            } else if c == '.' && !is_floating {
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

        let _starts_with_dot = string.chars().nth(0) == Some('.');
        let _ends_with_dot = string.chars().rev().nth(0) == Some('.');
        // if starts_with_dot && ends_with_dot {
        //     panic!("Number cannot start and end with a dot.");
        // }

        self.position.column += (string.len() - 1) as u32;

        if is_floating {
            let error_msg = format!("Could not parse floating point number: {}", string);
            TokenKind::Literal(LiteralKind::Floating(
                string.parse::<f64>().expect(&error_msg),
            ))
        } else {
            let error_msg = format!("Could not parse integer number: {}", string);
            TokenKind::Literal(LiteralKind::Integer(
                string.parse::<u128>().expect(&error_msg),
            ))
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
            Keyword(kw) => self.position.column += (kw.get_length() - 1) as u32,
            Identifier(id) => self.position.column += (id.len() - 1) as u32,
            _ => panic!("Identifier not a keyword or identifier."),
        };
        token
    }

    fn peek(&self, n: usize) -> Option<char> {
        self.chars.clone().nth(n).as_ref().copied()
    }
    fn advance(&mut self) {
        self.chars.next();
    }
}
#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use crate::token::{KeywordKind, LiteralKind, PunctuatorKind, TokenKind};

    #[test]
    fn test_0() {
        let source = "var a = 2222;";

        let mut lexer = Lexer::new();
        let tokens = lexer.scan_tokens(source);

        assert_eq!(tokens[0].kind, TokenKind::Keyword(KeywordKind::Var));
        assert_eq!(tokens[1].kind, TokenKind::Identifier(String::from("a")));
        assert_eq!(tokens[2].kind, TokenKind::Punctuator(PunctuatorKind::Equal));
        assert_eq!(
            tokens[3].kind,
            TokenKind::Literal(LiteralKind::Integer(2222))
        );
        assert_eq!(
            tokens[4].kind,
            TokenKind::Punctuator(PunctuatorKind::Semicolon)
        );
    }
}
