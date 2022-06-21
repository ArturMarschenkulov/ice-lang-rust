use crate::token::*;
use std::str::Chars;

fn is_digit(c: char) -> bool {
    // c >= '0' && c <= '9'
    ('0'..='9').contains(&c)
}
fn is_alpha(c: char) -> bool {
    // (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
    let minor_case = ('a'..='z').contains(&c);
    let major_case = ('A'..='Z').contains(&c);
    let underscore = c == '_';
    minor_case || major_case || underscore
}
fn is_alpha_numeric(c: char) -> bool {
    is_digit(c) || is_alpha(c)
}
fn is_skip_token(token: &Token) -> bool {
    use SpecialKeywordKind::*;
    use TokenKind::*;
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
    Tokenizer::new().scan_tokens(source)
}
// #[derive(Debug, Clone, Copy)]
// struct Position {
//     line: u32,
//     column: u32,
// }
// #[derive(Debug)]
// struct Span {
//     start: Position,
//     end: Position,
// }
struct Tokenizer<'a> {
    chars: Chars<'a>,
    position: Position,
}

impl<'a> Tokenizer<'a> {
    fn new() -> Self {
        Self {
            chars: "".chars(),
            position: Position { line: 1, column: 1 },
        }
    }
    fn scan_tokens(&mut self, source: &'a str) -> Vec<Token> {
        self.chars = source.chars();

        // The idea is to have two passes. The first pass basically gets the raw tokens.
        // Meaning there are no composite tokens (e.g. ==), whitespaces and comments are included, etc.
        // The second pass glues together the tokens into composite tokens and handles comments etc.
        // Of course, one can do the same thing in only one pass, which would be more effificient,
        // but this way the structure is easier to modify. This might help in implementing custom operators.

        let mut raw_tokens = Vec::new();
        while let Some(ch) = self.peek(0) {
            let token = self.scan_token(ch);

            if &token.kind == &TokenKind::SpecialKeyword(Newline) {
                self.position.line += 1;
                self.position.column = 1;
            } else {
                self.position.column += 1;
            }

            if !is_skip_token(&token) {
                raw_tokens.push(token);
            }
            self.advance();
        }

        let mut end_tokens = Vec::new();
        let mut iter_token = raw_tokens.iter();
        let mut num_to_skip = 0;
        while let Some(token_0) = iter_token.nth(num_to_skip) {
            let token_1 = iter_token.clone().nth(0);
            //let token_2 = iter_token.clone().nth(1);

            let max_possible_tokens;
            match token_1 {
                None => max_possible_tokens = 1,
                Some(..) => max_possible_tokens = 2,
            };

            let glued_token = if max_possible_tokens == 2 {
                let token_1 = token_1.unwrap();
                if is_gluable(&[token_0, token_1]) {
                    num_to_skip = 1;
                    glue(&[token_0, token_1])
                } else {
                    num_to_skip = 0;
                    token_0.clone()
                }
            } else if max_possible_tokens == 1 {
                num_to_skip = 0;
                token_0.clone()
            } else {
                panic!("This should never happen");
            };
            end_tokens.push(glued_token);
        }

        use SpecialKeywordKind::*;
        use TokenKind::*;
        end_tokens.push(Token {
            kind: SpecialKeyword(Eof),
            span: Span {
                start: Position { line: 0, column: 0 },
                end: Position { line: 0, column: 0 },
            },
        });
        end_tokens
    }
    fn scan_token(&mut self, c: char) -> Token {
        let token = self.get_token(c);
        token
        // let token_kind = self.determine_token_type(c);

        // self.create_token_from_type(token_kind)
    }

    // fn create_token_from_type(&mut self, kind: TokenKind) -> Token {
    //     match kind {
    //         _ => Token { kind },
    //     }
    // }

    fn get_token(&mut self, c: char) -> Token {
        use PunctuatorKind::*;
        use SpecialKeywordKind::*;
        use TokenKind::*;
        let start_pos = self.position;

        let token_kind = match c {
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
            '"' => self.lex_string(),
            cc if is_digit(cc) => self.lex_number(),
            cc if is_alpha(cc) => self.lex_identifier(),
            cc => panic!("This is an unknown character {:?}.", cc),
        };
        Token {
            kind: token_kind,
            span: Span {
                start: start_pos,
                end: self.position,
            },
        }
    }
    fn lex_escape_char(&mut self) -> String {
        let mut string_content = String::new();
        if self.peek(1) == Some('\\') {
            let mut is_escape = true;
            match self.peek(2) {
                Some('n') => string_content.push('\n'),
                Some('t') => string_content.push('\t'),
                Some('0') => string_content.push('\0'),
                Some('\\') => string_content.push('\\'),
                Some('\"') => string_content.push('\"'),
                Some('\'') => string_content.push('\''),
                _ => is_escape = false,
            }
            if is_escape {
                self.advance();
                self.advance();
                self.position.column += 2;
            }
        }
        string_content
    }
    fn lex_string(&mut self) -> TokenKind {
        // TODO: Are the delimiter of a string, part of its span? Right now they are. Think about it.
        let mut string_content = String::new();
        while let Some(c) = self.peek(1) {
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
        self.advance(); // this is for the '"'

        if self.peek(0) == None {
            panic!("Unterminated string.");
        }
        TokenKind::Literal(LiteralKind::String(string_content))
    }
    fn lex_comment_line(&mut self) -> TokenKind {
        self.advance(); // for '//'
        while self.peek(1) != Some('\n') && self.peek(1) != Some('\0') {
            self.advance();
            self.position.column += 1;
        }
        TokenKind::SpecialKeyword(SpecialKeywordKind::Comment)
    }
    fn lex_comment_block(&mut self) -> TokenKind {
        // TODO: Implement nested block comments
        self.advance(); // for '/'
        self.advance(); // for '*'
        self.position.column += 2;

        while self.peek(1) != Some('*') && self.peek(2) != Some('/') {
            self.advance();

            if self.peek(1) == Some('\n') {
                self.position.line += 1;
                self.position.column = 1;
            } else {
                self.position.column += 1;
            }
            self.position.column += 1;
        }
        self.advance(); // for '*'
        self.advance(); // for '/'
        self.position.column += 2;
        TokenKind::SpecialKeyword(SpecialKeywordKind::Comment)
    }
    fn lex_number(&mut self) -> TokenKind {
        use LiteralKind::*;
        use TokenKind::*;

        // Because LiteralKind has `String` as a field, we have to fully qualify this `String`.
        let mut string_content = std::string::String::new();
        string_content.push(self.peek(0).unwrap());
        while let Some(c) = self.peek(1) {
            if is_digit(c) {
                string_content.push(c);
                self.advance();
            } else {
                break;
            }
        }

        let number = string_content.parse::<i64>().unwrap();
        Literal(Integer(number))
    }
    fn lex_identifier(&mut self) -> TokenKind {
        let mut string_content = String::new();
        let peeked_char = self.peek(0).unwrap();
        string_content.push(peeked_char);

        while let Some(c) = self.peek(1) {
            if is_alpha_numeric(c) {
                string_content.push(c);
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

    fn peek(&mut self, n: usize) -> Option<char> {
        // if let Some(ch) = &self.chars.clone().nth(n) {
        //     Some(*ch)
        // } else {
        //     None
        // }

        // self.chars.clone().nth(n).as_ref().map(|ch| *ch)

        self.chars.clone().nth(n).as_ref().copied()
    }
    fn advance(&mut self) {
        self.chars.next();
    }
}
#[cfg(test)]
mod test {
    use crate::token::{KeywordKind, LiteralKind, PunctuatorKind, TokenKind};
    use crate::tokenizer::Tokenizer;

    #[test]
    fn test_0() {
        let source = "var a = 2222;";

        let mut lexer = Tokenizer::new();
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
