#![cfg(test)]

use super::token::{KeywordKind, LiteralKind, NumberBase, PunctuatorKind, TokenKind};
use crate::lexer::Lexer;

use super::*;

/// Helper function to convert a string to a vector of token kinds.
fn to_token_kinds(s: &str) -> Vec<TokenKind> {
    Lexer::from(s)
        .scan_tokens()
        .iter()
        .map(|t| t.kind.clone())
        .collect::<Vec<_>>()
}

/// Helper function to convert a string to a vector of token spans.
fn to_token_spans(s: &str) -> Vec<token::Span> {
    Lexer::from(s)
        .scan_tokens()
        .iter()
        .map(|t| t.span)
        .collect::<Vec<_>>()
}

mod cursor {
    use super::*;
    #[test]
    fn test_peek() {
        let txt = "a";
        let lexer = Lexer::from(txt);
        assert_eq!(lexer.peek(0), Some('a'));
        assert_eq!(lexer.peek(1), None);

        let txt = "abc";
        let mut lexer = Lexer::from(txt);
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
        let lexer = Lexer::from(txt);
        assert_eq!(lexer.check_with(0, |x| x == &'a'), Ok('a'));
        assert_eq!(lexer.check_with(0, |x| x == &'b'), Err(Some('a')));
        assert_eq!(lexer.check_with(1, |x| x == &'a'), Err(None));
        assert_eq!(lexer.check_with(1, |x| x == &'b'), Err(None));
        assert_eq!(lexer.check_with(-1, |x| x == &'a'), Err(None));
        assert_eq!(lexer.check_with(-1, |x| x == &'b'), Err(None));

        let txt = "abc";
        let mut lexer = Lexer::from(txt);
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
    fn test_eat_with() {
        let txt = "a";
        let mut lexer = Lexer::from(txt);
        assert_eq!(lexer.eat_with(|x| x == &'b'), Err(Some('a')));
        assert_eq!(lexer.eat_with(|x| x == &'a'), Ok('a'));
        assert_eq!(lexer.eat_with(|x| x == &'b'), Err(None));
        assert_eq!(lexer.eat_with(|x| x == &'a'), Err(None));

        let txt = "abc";
        let mut lexer = Lexer::from(txt);
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
        let mut lexer = Lexer::from(txt);
        assert_eq!(lexer.eat_while(|x| x == &'a'), "a".to_string());
        assert_eq!(lexer.eat_while(|x| x.is_alphabetic()), "bc".to_string());
        assert_eq!(lexer.eat_while(|x| x.is_alphanumeric()), "504".to_string());
        assert_eq!(lexer.eat_while(|x| x.is_numeric()), "".to_string());
    }
    #[test]
    fn test_eat_str() {
        let txt = "abc";
        let mut lexer = Lexer::from(txt);
        assert_eq!(lexer.eat_str("bc"), None);
        assert_eq!(lexer.eat_str("ab"), Some("ab".to_string()));
        assert_eq!(lexer.eat_str("bc"), None);
        assert_eq!(lexer.eat_str("c"), Some("c".to_string()));
    }
}

#[test]
fn test_scan_tokens() {
    use token::*;
    use KeywordKind::*;
    use Whitespace::*;

    use TokenKind::*;
    assert_eq!(
        Lexer::from("fn").scan_tokens(),
        vec![
            Token::new(Keyword(Fn), Span::from(((1, 1), (1, 2))), NoBoth),
            Token::new(
                SpecialKeyword(SpecialKeywordKind::Eof),
                Span::from(((1, 3), (1, 3))),
                Both
            ),
        ]
    );
}
#[test]
fn test_scan_token_kind() {
    use SpecialKeywordKind::*;
    use TokenKind::*;
    let txt = "";
    let mut lexer = Lexer::from(txt);
    assert_eq!(lexer.scan_token().map(|x| x.kind), Ok(SpecialKeyword(Eof)));
}

#[test]
fn test_lex_char() {
    use crate::lexer::Error;
    use LiteralKind::*;
    use TokenKind::*;

    // Simple cases
    let txt = r#"'a'"#;
    let mut lexer = Lexer::from(txt);
    assert_eq!(lexer.lex_char(), Ok(Literal(Char('a'))));

    // Edge cases
    let txt = r#"'\\'"#;
    let mut lexer = Lexer::from(txt);
    assert_eq!(lexer.lex_char(), Ok(Literal(Char('\\'))));

    let txt = r#"'\''"#;
    let mut lexer = Lexer::from(txt);
    assert_eq!(lexer.lex_char(), Ok(Literal(Char('\''))));

    // Handling errors

    // Empty literal
    let txt = r#"''"#;
    let mut lexer = Lexer::from(txt);
    assert_eq!(lexer.lex_char(), Err(Error::empty_char_literal()));

    // Newline in literal
    let txt = r#"'
    '"#;
    let mut lexer = Lexer::from(txt);
    assert_eq!(lexer.lex_char(), Err(Error::new_line_in_char_lit()));

    let txt = r#"'ab'"#;
    let mut lexer = Lexer::from(txt);
    assert_eq!(
        lexer.lex_char(),
        Err(Error::char_lit_contains_multiple_codepoints())
    );

    let txt = r#"'\"#;
    let mut lexer = Lexer::from(txt);
    assert_eq!(lexer.lex_char(), Err(Error::unterminated_char_lit()));

    // let txt = r#"'a"#;
    // let mut lexer = Lexer::new_from_str(txt);
    // assert_eq!(lexer.lex_char(), Err(LexerError::unterminated_char_lit()));
}
#[test]
fn test_lex_str() {}
#[test]
fn test_comment_line() {
    let txt = "//";
    let _ = Lexer::from(txt).scan_tokens();

    let txt = "// This is a comment";
    let _ = Lexer::from(txt).scan_tokens();

    let txt = "// This is a comment\n// And this too\n";
    let _ = Lexer::from(txt).scan_tokens();
}
#[test]
fn test_comment_block() {
    let txt = "/* This is a line comment*/";
    let _ = Lexer::from(txt).scan_tokens();
}

#[test]
fn num_0_() {
    let txt = "0";
    let _ = Lexer::from(txt).scan_tokens();
}

#[test]

fn test_lex_number() {
    use crate::lexer::Error;
    use NumberBase::*;
    use TokenKind::*;

    // Simple cases
    // Decimal numbers

    assert_eq!(
        Lexer::from("0").lex_number(),
        Ok(Literal(LiteralKind::integer("0", None, None)))
    );

    assert_eq!(
        Lexer::from("1").lex_number(),
        Ok(Literal(LiteralKind::integer("1", None, None)))
    );

    assert_eq!(
        Lexer::from("123").lex_number(),
        Ok(Literal(LiteralKind::integer("123", None, None)))
    );

    assert_eq!(
        Lexer::from("123456").lex_number(),
        Ok(Literal(LiteralKind::integer("123456", None, None)))
    );

    // Error cases
    // Invalid binary number
    assert_eq!(
        Lexer::from("0b22").lex_number(),
        Err(Error::invalid_digit_base_prefix(Some(Binary)))
    );
    assert_eq!(
        Lexer::from("0o8").lex_number(),
        Err(Error::invalid_digit_base_prefix(Some(Octal)))
    );
    // assert_eq!(
    //     Lexer::new_from_str("0xg").lex_number(),
    //     Err(LexerError::invalid_digit_base_prefix(Some(Hexadecimal)))
    // );

    assert_eq!(
        Lexer::from("0x0.0").lex_number(),
        Err(Error::floats_dont_have_base_prefix(Some(Hexadecimal)))
    );

    // assert_eq!(
    //     Lexer::new_from_str("0da").lex_number(),
    //     Err(LexerError::invalid_digit_base_prefix(Some(Decimal)))
    // );

    assert!(Lexer::from("0.").lex_number().is_err());

    // assert_eq!(
    //     Lexer::new_from_str("0b").lex_number(),
    //     Err(LexerError::invalid_digit_base_prefix(Some(Binary)))
    // );
}

mod num {
    use super::*;

    #[test]
    #[should_panic]
    fn invalid_float() {
        let txt = "0.";
        let t = Lexer::from(txt).scan_tokens();
        assert_ne!(t.len() - 1, 1);
    }

    #[test]
    fn t1() {
        use PunctuatorKind::*;
        use TokenKind::*;
        let txt = ".0";
        let t = Lexer::from(txt).scan_tokens();
        assert_eq!(t.len() - 1, 2);
        assert_eq!(t[0].kind, Punctuator(Dot));
    }
}
#[test]
fn test_scan_tokens_KIND() {
    use token::*;
    use KeywordKind::*;
    use LiteralKind::*;
    use PunctuatorKind::*;
    use TokenKind::*;

    assert_eq!(
        to_token_kinds("var a := 2222;"),
        vec![
            Keyword(Var),
            Identifier(String::from("a")),
            Punctuator(Colon),
            Punctuator(Equal),
            Literal(Number(token::Number::integer("2222", None, None))),
            Punctuator(Semicolon),
            SpecialKeyword(SpecialKeywordKind::Eof)
        ]
    );
    assert_eq!(
        to_token_kinds(r#"var a := "hello";"#),
        vec![
            Keyword(Var),
            Identifier(String::from("a")),
            Punctuator(Colon),
            Punctuator(Equal),
            Literal(Str("hello".to_owned())),
            Punctuator(Semicolon),
            SpecialKeyword(SpecialKeywordKind::Eof)
        ]
    );
    assert_eq!(
        to_token_kinds(r#"var a := ' ';"#),
        vec![
            Keyword(Var),
            Identifier(String::from("a")),
            Punctuator(Colon),
            Punctuator(Equal),
            Literal(Char(' ')),
            Punctuator(Semicolon),
            SpecialKeyword(SpecialKeywordKind::Eof)
        ]
    );

    assert_eq!(
        to_token_kinds(
            r#"var a := ' ';
        var a := ' ';"#
        ),
        vec![
            Keyword(Var),
            Identifier(String::from("a")),
            Punctuator(Colon),
            Punctuator(Equal),
            Literal(Char(' ')),
            Punctuator(Semicolon),
            Keyword(Var),
            Identifier(String::from("a")),
            Punctuator(Colon),
            Punctuator(Equal),
            Literal(Char(' ')),
            Punctuator(Semicolon),
            SpecialKeyword(SpecialKeywordKind::Eof)
        ]
    );

    assert_eq!(
        to_token_kinds("std::io::str;"),
        vec![
            Identifier("std".to_owned()),
            Punctuator(ColonColon),
            Identifier("io".to_owned()),
            Punctuator(ColonColon),
            Identifier("str".to_owned()),
            Punctuator(Semicolon),
            SpecialKeyword(SpecialKeywordKind::Eof)
        ]
    );
}
#[test]
fn test_scan_tokens_SPAN() {

    assert_eq!(
        to_token_spans("var a := 2222;"),
        vec![
            Span::from(((1, 1), (1, 3))),   // var
            Span::from(((1, 5), (1, 5))),   // a
            Span::from(((1, 7), (1, 7))),   // :
            Span::from(((1, 8), (1, 8))),   // =
            Span::from(((1, 10), (1, 13))), // 2222
            Span::from(((1, 14), (1, 14))), // ;
            Span::from(((1, 15), (1, 15))), // eof
        ]
    );
    assert_eq!(
        to_token_spans(r#"var a := "hello";"#),
        vec![
            Span::from(((1, 1), (1, 3))),   // var
            Span::from(((1, 5), (1, 5))),   // a
            Span::from(((1, 7), (1, 7))),   // :
            Span::from(((1, 8), (1, 8))),   // =
            Span::from(((1, 10), (1, 16))), // "hello"
            Span::from(((1, 17), (1, 17))), // ;
            Span::from(((1, 18), (1, 18))), // eof
        ]
    );
    // assert_eq!(
    //     tokens(
    //         r#"var a := ' ';
    //     var a := ' ';"#
    //     ),
    //     vec![
    //         Span::from(((1, 1), (1, 3))),   // var
    //         Span::from(((1, 5), (1, 5))),   // a
    //         Span::from(((1, 7), (1, 7))),   // :
    //         Span::from(((1, 8), (1, 8))),   // =
    //         Span::from(((1, 10), (1, 12))), // ' '
    //         Span::from(((1, 13), (1, 13))), // ;
    //         Span::from(((2, 1), (2, 3))),   // var
    //         Span::from(((2, 5), (2, 5))),   // a
    //         Span::from(((2, 7), (2, 7))),   // :
    //         Span::from(((2, 8), (2, 8))),   // =
    //         Span::from(((2, 10), (2, 12))), // ' '
    //         Span::from(((2, 13), (2, 13))), // ;
    //         Span::from(((2, 14), (2, 14))), // eof
    //     ]
    // );
    assert_eq!(
        to_token_spans(r#"var a := ' ';"#),
        vec![
            Span::from(((1, 1), (1, 3))),   // var
            Span::from(((1, 5), (1, 5))),   // a
            Span::from(((1, 7), (1, 7))),   // :
            Span::from(((1, 8), (1, 8))),   // =
            Span::from(((1, 10), (1, 12))), // ' '
            Span::from(((1, 13), (1, 13))), // ;
            Span::from(((1, 14), (1, 14))), // eof
        ]
    );

    assert_eq!(
        to_token_spans("std::io::str;"),
        vec![
            Span::from(((1, 1), (1, 3))),   // std
            Span::from(((1, 4), (1, 5))),   // ::
            Span::from(((1, 6), (1, 7))),   // io
            Span::from(((1, 8), (1, 9))),   // ::
            Span::from(((1, 10), (1, 12))), // str
            Span::from(((1, 13), (1, 13))), // ;
            Span::from(((1, 14), (1, 14))), // eof
        ]
    );
}

#[test]
fn test_complex_token_cooking() {
    use token::*;
    use PunctuatorKind::*;
    use TokenKind::*;

    assert_eq!(
        to_token_kinds(".."),
        vec![
            Punctuator(PunctuatorKind::Complex(vec![
                PunctuatorKind::Dot,
                PunctuatorKind::Dot
            ])),
            SpecialKeyword(SpecialKeywordKind::Eof),
        ],
    );

    assert_eq!(
        to_token_kinds("++"),
        vec![
            Punctuator(PunctuatorKind::Complex(vec![
                PunctuatorKind::Plus,
                PunctuatorKind::Plus
            ])),
            SpecialKeyword(SpecialKeywordKind::Eof),
        ],
    );

    assert_eq!(
        to_token_kinds("=."),
        vec![
            Punctuator(Equal),
            Punctuator(Dot),
            SpecialKeyword(SpecialKeywordKind::Eof),
        ],
    );

    assert_eq!(
        to_token_kinds(".=."),
        vec![
            Punctuator(Dot),
            Punctuator(Equal),
            Punctuator(Dot),
            SpecialKeyword(SpecialKeywordKind::Eof),
        ],
    );

    assert_eq!(
        to_token_kinds(":=:"),
        vec![
            Punctuator(Colon),
            Punctuator(Equal),
            Punctuator(Colon),
            SpecialKeyword(SpecialKeywordKind::Eof),
        ],
    );
    assert_eq!(
        to_token_kinds(":="),
        vec![
            Punctuator(Colon),
            Punctuator(Equal),
            SpecialKeyword(SpecialKeywordKind::Eof),
        ],
    );
}
