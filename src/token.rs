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
impl KeywordKind {
    pub fn get_length(&self) -> usize {
        use KeywordKind::*;
        match self {
            Var => "var".len(),
            Fn => "fn".len(),

            If => "if".len(),
            Else => "else".len(),
            While => "while".len(),
            For => "for".len(),

            True => "true".len(),
            False => "false".len(),

            Print => "print".len(),
            Println => "println".len(),
        }
    }
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
fn unite_to_composite(tk: &[&TokenKind]) -> TokenKind {
    use PunctuatorKind::*;
    use TokenKind::*;
    match &tk {
        [Punctuator(Plus), Punctuator(Equal)] => Punctuator(PlusEqual),
        [Punctuator(Minus), Punctuator(Equal)] => Punctuator(MinusEqual),
        [Punctuator(Star), Punctuator(Equal)] => Punctuator(StarEqual),
        [Punctuator(Slash), Punctuator(Equal)] => Punctuator(SlashEqual),
        [Punctuator(Equal), Punctuator(Equal)] => Punctuator(EqualEqual),
        [Punctuator(Bang), Punctuator(Equal)] => Punctuator(BangEqual),
        [Punctuator(Ampersand), Punctuator(Ampersand)] => Punctuator(AmpersandAmpersand),
        [Punctuator(Pipe), Punctuator(Pipe)] => Punctuator(PipePipe),
        [Punctuator(Greater), Punctuator(Equal)] => Punctuator(GreaterEqual),
        [Punctuator(Less), Punctuator(Equal)] => Punctuator(LessEqual),
        [Punctuator(Minus), Punctuator(Greater)] => Punctuator(MinusGreater),
        _ => panic!("unexpected token kind"),
    }
}
/// Glues together simple tokens into composite tokens.
pub fn glue(t: &[&Token]) -> Token {
    let token = match t {
        [t_0, t_1] => {
            if is_gluable(t) {
                let kinds = &[&t_0.kind, &t_1.kind];
                Token {
                    kind: unite_to_composite(kinds),
                    span: Span {
                        start: t_0.span.start,
                        end: t_1.span.end,
                    },
                }
            } else {
                panic!("unexpected token kind")
            }
        }
        _ => unimplemented!(),
    };
    token
}
pub fn is_gluable(t: &[&Token]) -> bool {
    use PunctuatorKind::*;
    use TokenKind::*;
    match t {
        [t_0, t_1] => {
            let kinds = (&t_0.kind, &t_1.kind);
            match kinds {
                (Punctuator(Plus), Punctuator(Plus)) => true,
                (Punctuator(Minus), Punctuator(Minus)) => true,
                (Punctuator(Equal), Punctuator(Equal)) => true,
                cc => false,
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
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Whitespace {
    left: bool,
    right: bool,
}
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    //pub whitespace: Whitespace,
}
