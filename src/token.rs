#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum TokenKind {
    Punctuator(PunctuatorKind),
    Literal(LiteralKind),
    Identifier(String),
    Keyword(KeywordKind),
    SpecialKeyword(SpecialKeywordKind),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PunctuatorKind {
    // Punctuator
    Plus,      // +
    Minus,     // -
    Star,      // *
    Slash,     // /
    Equal,     // =
    Bang,      // !
    Greater,   // >
    Less,      // <
    Ampersand, // &
    Pipe,      // |
    Colon,     // :
    Semicolon, // ;
    Percent,   // %
    Dollar,    // $
    Question,  // ?
    Hash,      // #
    Dot,       // .
    Comma,     // ,
    Backslash, // \
    At,        // @

    LeftParen,    // (
    RightParen,   // )
    LeftBracket,  // [
    RightBracket, // ]
    LeftBrace,    // {
    RightBrace,   // }

    // Punctuators two
    PlusEqual,  // +=
    MinusEqual, // -=
    StarEqual,  // *=
    SlashEqual, // /=

    EqualEqual,         // ==
    BangEqual,          // !=
    AmpersandAmpersand, // &&
    PipePipe,           // ||
    GreaterEqual,       // >=
    LessEqual,          // <=

    MinusGreater, // ->
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum KeywordKind {
    Var,
    Fn,

    If,
    Else,
    While,
    For,

    True,
    False,

    Print,
    Println,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SpecialKeywordKind {
    Eof,
    Newline,
    Whitespace,
    Comment, //Unknown,
}

//FnDeclaration(Token, Vec<Token>, Box<Expr>),

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LiteralKind {
    Integer(i64),
    //Float(f64),
    Boolean(bool),
    //Char(char)
    String(String),
    Unit,
}

fn is_composite_token_kind(token_kind: &TokenKind) -> bool {
    use PunctuatorKind::*;
    use TokenKind::*;
    match token_kind {
        Punctuator(PlusEqual)
        | Punctuator(MinusEqual)
        | Punctuator(StarEqual)
        | Punctuator(SlashEqual)
        | Punctuator(EqualEqual)
        | Punctuator(BangEqual)
        | Punctuator(AmpersandAmpersand)
        | Punctuator(PipePipe)
        | Punctuator(GreaterEqual)
        | Punctuator(LessEqual)
        | Punctuator(MinusGreater) => true,

        _ => false,
    }
}
fn unglue(t: &Token) -> Vec<Token> {
    unimplemented!()
}

/// Glues together simple tokens into composite tokens.
pub fn glue(t: &[&Token]) -> Token {
    use PunctuatorKind::*;
    use TokenKind::*;
    let token = match t {
        [t_0, t_1] => {
            let kinds = (&t_0.kind, &t_1.kind);
            match kinds {
                (Punctuator(Plus), Punctuator(Plus)) => Token {
                    kind: Punctuator(PlusEqual),
                },
                (Punctuator(Minus), Punctuator(Minus)) => Token {
                    kind: Punctuator(MinusEqual),
                },
                (Punctuator(Equal), Punctuator(Equal)) => Token {
                    kind: Punctuator(EqualEqual),
                },
                pp => panic!("glue: {:?}", pp),
            }
        }
        _ => unimplemented!(),
    };
    token
}
pub fn is_gluable(t: &[&Token]) -> bool {
    use PunctuatorKind::*;
    use TokenKind::*;
    println!("is_gluable                  param: {:?}", &t);
    match t {
        [t_0, t_1] => {
            let kinds = (&t_0.kind, &t_1.kind);
            match kinds {
                (Punctuator(Plus), Punctuator(Plus)) => true,
                (Punctuator(Minus), Punctuator(Minus)) => true,
                (Punctuator(Equal), Punctuator(Equal)) => true,
                cc => {
                    //panic!("is_gluable: {:?}", cc);
                    false
                }
            }
        }
        cc => {
            panic!("is_gluable: {:?}", cc);
            false
        }
    }
}
fn is_composite_token(token: &Token) -> bool {
    is_composite_token_kind(&token.kind)
}

//struct Position {
//    line: u32,
//    column: u32,
//}
//struct Span {
//    start: Position,
//    end: Position,
//}
struct Whitespace {
    left: bool,
    right: bool,
}
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Token {
    pub kind: TokenKind,
    //pub whitespace: Whitespace,
    //span: Span,
}
