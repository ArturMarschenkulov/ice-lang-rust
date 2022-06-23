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
            Punctuator(p) => !matches!(
                p,
                Semicolon
                    | Comma
                    | Colon
                    | LeftBrace
                    | RightBrace
                    | LeftBracket
                    | RightBracket
                    | LeftParen
                    | RightParen
            ),
            _ => false,
        }
    }
    pub fn simplify(self) -> TokenKind {
        use PunctuatorKind::*;
        use TokenKind::*;
        match &self {
            Punctuator(Complex(c)) => match *c.as_slice() {
                [Equal, Equal] => Punctuator(EqualEqual),
                [Bang, Equal] => Punctuator(BangEqual),
                [Greater, Equal] => Punctuator(GreaterEqual),
                [Less, Equal] => Punctuator(LessEqual),
                [Ampersand, Ampersand] => Punctuator(AmpersandAmpersand),
                [Pipe, Pipe] => Punctuator(PipePipe),

                [Minus, Greater] => Punctuator(MinusGreater),

                [Plus, Equal] => Punctuator(PlusEqual),
                [Minus, Equal] => Punctuator(MinusEqual),
                [Star, Equal] => Punctuator(StarEqual),
                [Slash, Equal] => Punctuator(SlashEqual),
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

/// Tokens a slice of tokens and converts them into a complex token.
/// Certain complex tokens are converted into a single token, which significes several tokens.
/// If token slice has only one token, the singular token itself is returned.
pub fn conv_to_complex(tokens: &[Token]) -> Token {
    use PunctuatorKind::*;
    use TokenKind::*;
    let tok_kinds = tokens
        .iter()
        .map(|token| {
            if let Punctuator(kind) = token.kind.clone() {
                kind
            } else {
                unreachable!()
            }
        })
        .collect::<Vec<_>>();

    match tokens.len() {
        0 => unreachable!(),
        1 => tokens.first().unwrap().clone(),
        _ => {
            let kind = Punctuator(Complex(tok_kinds)).simplify();

            let first_tok = tokens.first().unwrap();
            let last_tok = tokens.last().unwrap();
            Token {
                kind: kind,
                span: Span {
                    start: first_tok.span.start,
                    end: last_tok.span.end,
                },
                whitespace: Whitespace::Undefined,
            }
        }
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Whitespace {
    None,
    Left,
    Right,
    Both,
    Undefined,
}
impl Whitespace {
    pub fn from(bools: (bool, bool)) -> Self {
        use Whitespace::*;
        match bools {
            (true, true) => Both,
            (true, false) => Right,
            (false, true) => Left,
            (false, false) => None,
        }
    }
    pub fn as_bools(self) -> (bool, bool) {
        use Whitespace::*;
        match self {
            None => (false, false),
            Left => (true, false),
            Right => (false, true),
            Both => (true, true),
            Undefined => unreachable!(),
        }
    }
    /// Creates the whitespace from given tokens. Those tokens are wrapped in an `Option`, to make handling first
    /// and last tokens easier for the caller
    pub fn from_tokens(left_token: Option<&Token>, right_token: Option<&Token>) -> Self {
        unimplemented!()
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub whitespace: Whitespace,
}

impl Token {
    pub fn is_skip_token(&self) -> bool {
        use SpecialKeywordKind::*;
        use TokenKind::*;
        matches!(
            self.kind,
            SpecialKeyword(Whitespace) | SpecialKeyword(Newline) | SpecialKeyword(Comment)
        )
    }
    /// Whether an operator is a prefix, postfix or infix operator, depends heavily on the whitespace it is surrounded by.
    ///
    fn is_token_operator_whitespace(&self) -> bool {
        use TokenKind::*;
        unimplemented!()
    }
}
