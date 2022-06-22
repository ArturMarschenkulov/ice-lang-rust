#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum TokenKind {
    Punctuator(PunctuatorKind),
    Literal(LiteralKind),
    Identifier(String),
    Keyword(KeywordKind),
    SpecialKeyword(SpecialKeywordKind),
}

impl TokenKind {
    pub fn can_be_part_of_complex(&self) -> bool {
        use PunctuatorKind::*;
        use TokenKind::*;
        match self {
            Punctuator(p) => match p {
                Semicolon | Comma => false,
                _ => true,
            },
            _ => false,
        }
    }
    pub fn simplify(self) -> TokenKind {
        use PunctuatorKind::*;
        use TokenKind::*;
        match &self {
            Punctuator(Complex(c)) => match c.as_slice() {
                &[Equal, Equal] => Punctuator(EqualEqual),
                &[Bang, Equal] => Punctuator(BangEqual),
                &[Greater, Equal] => Punctuator(GreaterEqual),
                &[Less, Equal] => Punctuator(LessEqual),
                &[Ampersand, Ampersand] => Punctuator(AmpersandAmpersand),
                &[Pipe, Pipe] => Punctuator(PipePipe),

                &[Minus, Greater] => Punctuator(MinusGreater),

                &[Plus, Equal] => Punctuator(PlusEqual),
                &[Minus, Equal] => Punctuator(MinusEqual),
                &[Star, Equal] => Punctuator(StarEqual),
                &[Slash, Equal] => Punctuator(SlashEqual),
                _ => self.clone(),
            },
            _ => self.clone(),
        }
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
    Complex(Vec<PunctuatorKind>),
    EqualEqual,         // ==
    BangEqual,          // !=
    MinusGreater,       // ->
    AmpersandAmpersand, // &&
    PipePipe,           // ||

    GreaterEqual, // >=
    LessEqual,    // <=

    PlusEqual,  // +=
    MinusEqual, // -=
    StarEqual,  // *=
    SlashEqual, // /=
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
