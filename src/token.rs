#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum TokenKind {
    Punctuator(PunctuatorKind),
    Literal(LiteralKind),
    Identifier(String),
    Keyword(KeywordKind),
    SpecialKeyword(SpecialKeywordKind),
}

impl TokenKind {
    /// splits token kind into its components. Makes only sense with composite punctuators.
    pub fn list_subset(&self) -> Vec<TokenKind> {
        use PunctuatorKind::*;
        use TokenKind::*;
        match self.clone() {
            Punctuator(PlusEqual) => vec![Punctuator(Plus), Punctuator(Equal)],
            Punctuator(MinusEqual) => vec![Punctuator(Minus), Punctuator(Equal)],
            Punctuator(StarEqual) => vec![Punctuator(Star), Punctuator(Equal)],
            Punctuator(SlashEqual) => vec![Punctuator(Slash), Punctuator(Equal)],

            Punctuator(EqualEqual) => vec![Punctuator(Equal), Punctuator(Equal)],
            Punctuator(BangEqual) => vec![Punctuator(Bang), Punctuator(Equal)],

            Punctuator(AmpersandAmpersand) => vec![Punctuator(Ampersand), Punctuator(Ampersand)],
            Punctuator(PipePipe) => vec![Punctuator(Pipe), Punctuator(Pipe)],

            Punctuator(GreaterEqual) => vec![Punctuator(Greater), Punctuator(Equal)],
            Punctuator(LessEqual) => vec![Punctuator(Less), Punctuator(Equal)],

            Punctuator(MinusGreater) => vec![Punctuator(Minus), Punctuator(Greater)],

            Punctuator(p) => vec![Punctuator(p)],
            Literal(l) => vec![Literal(l)],
            Identifier(s) => vec![Identifier(s)],
            Keyword(k) => vec![Keyword(k)],
            SpecialKeyword(sk) => vec![SpecialKeyword(sk)],
        }
    }
    /// This function returns all the token kinds this token kind can be part of. This also includes itself.
    pub fn list_superset(&self) -> Vec<Vec<TokenKind>> {
        use PunctuatorKind::*;
        use TokenKind::*;
        let result = match self.clone() {
            Punctuator(Plus) => vec![Punctuator(Plus), Punctuator(PlusEqual)],
            Punctuator(Minus) => vec![
                Punctuator(Minus),
                Punctuator(MinusEqual),
                Punctuator(MinusGreater),
            ],
            Punctuator(Star) => vec![Punctuator(Star), Punctuator(StarEqual)],
            Punctuator(Slash) => vec![Punctuator(Slash), Punctuator(SlashEqual)],
            Punctuator(Equal) => vec![Punctuator(Equal), Punctuator(EqualEqual)],
            Punctuator(Bang) => vec![Punctuator(Bang), Punctuator(BangEqual)],
            Punctuator(Ampersand) => {
                vec![Punctuator(Ampersand), Punctuator(AmpersandAmpersand)]
            }
            Punctuator(Pipe) => vec![Punctuator(Pipe), Punctuator(PipePipe)],
            Punctuator(Greater) => vec![Punctuator(Greater), Punctuator(GreaterEqual)],
            Punctuator(Less) => vec![Punctuator(Less), Punctuator(LessEqual)],

            Punctuator(p) => vec![Punctuator(p)],
            Literal(l) => vec![Literal(l)],
            Identifier(i) => vec![Identifier(i)],
            Keyword(k) => vec![Keyword(k)],
            SpecialKeyword(sk) => vec![SpecialKeyword(sk)],
        };
        result.into_iter().map(|v| v.list_subset()).collect()
    }
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
    Integer(u128),
    //Float(f64),
    Boolean(bool),
    //Char(char)
    String(String),
    Unit,
}

fn is_composite_token_kind(token_kind: &TokenKind) -> bool {
    token_kind.list_subset().len() > 1
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
    match t {
        // If only one token is provided then simply an identical copy is returned.
        [t_0] => t_0.clone().clone(),
        [t_0, t_1] => {
            if is_gluable(t) {
                let kinds = &[&t_0.kind, &t_1.kind];
                Token {
                    kind: unite_to_composite(kinds),
                    span: Span {
                        start: t_0.span.start,
                        end: t_1.span.end,
                    },
                    whitespace: t_0.whitespace.clone(),
                }
            } else {
                panic!("unexpected token kind")
            }
        }
        _ => unimplemented!(
            "The logic for tokens consisting of more than 2 tokens is not yet implemented"
        ),
    }
}
pub fn is_gluable(tokens: &[&Token]) -> bool {
    use PunctuatorKind::*;
    use TokenKind::*;
    match tokens {
        [] => false,
        [_] => false,
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
            panic!("is_gluable: {:?}", cc)
        }
    }

    // TODO: Implement this function proberly
    // let len = tokens.len();
    // if len <= 1 {
    //     false;
    // } else {
    //     let first_token = &tokens[0];
    //     let superset = first_token.kind.list_superset();
    //     assert!(superset.len() == tokens.len())
    // }
    // false
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
pub enum Whitespace {
    None,
    Left,
    Right,
    Both,
    Undefined,
}
impl Whitespace {
    fn to_bools(&self) -> (bool, bool) {
        match self {
            Self::None => (false, false),
            Self::Left => (true, false),
            Self::Right => (false, true),
            Self::Both => (true, true),
            Self::Undefined => panic!(
                "{}",
                "'Undefined' should never be read, it is only a temporary value"
            ),
        }
    }
    fn from(bools: (bool, bool)) -> Self {
        match bools {
            (false, false) => Self::None,
            (true, false) => Self::Left,
            (false, true) => Self::Right,
            (true, true) => Self::Both,
        }
    }
    fn from_bools(&mut self, bools: (bool, bool)) {
        match bools {
            (true, true) => *self = Self::Both,
            (true, false) => *self = Self::Left,
            (false, true) => *self = Self::Right,
            (false, false) => *self = Self::None,
        }
    }
}
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub whitespace: Whitespace,
}
