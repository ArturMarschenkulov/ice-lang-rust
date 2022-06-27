#[derive(Debug, PartialEq, Clone)]
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
                    | Dot
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
    pub fn is_simple(&self) -> bool {
        use PunctuatorKind::*;
        use SpecialKeywordKind::*;
        use TokenKind::*;
        match self {
            Punctuator(p) => matches!(
                p,
                Plus | Minus
                    | Star
                    | Slash
                    | Equal
                    | Bang
                    | Greater
                    | Less
                    | Ampersand
                    | Pipe
                    | Colon
                    | Semicolon
                    | Percent
                    | Dollar
                    | Question
                    | Hash
                    | Dot
                    | Comma
                    | Backslash
                    | At
                    | LeftParen
                    | RightParen
                    | LeftBracket
                    | RightBracket
                    | LeftBrace
                    | RightBrace
            ),
            SpecialKeyword(Eof) | SpecialKeyword(Newline) | SpecialKeyword(Whitespace) => true,
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
    // pub fn complexify(self) -> TokenKind {
    //     use PunctuatorKind::*;
    //     use TokenKind::*;
    //     match &self {
    //         Punctuator(EqualEqual) => Punctuator(Complex(vec![Equal, Equal])),
    //         Punctuator(BangEqual) => Punctuator(Complex(vec![Bang, Equal])),
    //         Punctuator(GreaterEqual) => Punctuator(Complex(vec![Greater, Equal])),
    //         Punctuator(LessEqual) => Punctuator(Complex(vec![Less, Equal])),
    //         Punctuator(AmpersandAmpersand) => Punctuator(Complex(vec![Ampersand, Ampersand])),
    //         Punctuator(PipePipe) => Punctuator(Complex(vec![Pipe, Pipe])),
    //         Punctuator(MinusGreater) => Punctuator(Complex(vec![Minus, Greater])),
    //         Punctuator(PlusEqual) => Punctuator(Complex(vec![Plus, Equal])),
    //         Punctuator(MinusEqual) => Punctuator(Complex(vec![Minus, Equal])),
    //         Punctuator(StarEqual) => Punctuator(Complex(vec![Star, Equal])),
    //         Punctuator(SlashEqual) => Punctuator(Complex(vec![Slash, Equal])),
    //         _ => self.clone(),
    //     }
    // }
    // pub fn is_complex(&self) -> bool {
    //     use PunctuatorKind::*;
    //     use TokenKind::*;
    //     if let Punctuator(Complex(_)) = self.clone().complexify() {
    //         true
    //     } else {
    //         false
    //     }
    // }
    pub fn is_to_skip(&self) -> bool {
        use SpecialKeywordKind::*;
        use TokenKind::*;
        matches!(
            self,
            SpecialKeyword(Whitespace) | SpecialKeyword(Newline) | SpecialKeyword(Comment)
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
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

            // Those are actually literals, so they will get special treatement
            True => "true".len(),
            False => "false".len(),

            // They will be removed
            Print => "print".len(),
            Println => "println".len(),
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
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
            let token_kind = Punctuator(Complex(tok_kinds)).simplify();

            let first_tok = tokens.first().unwrap();
            let last_tok = tokens.last().unwrap();
            Token {
                kind: token_kind,
                span: Span {
                    start: first_tok.span.start,
                    end: last_tok.span.end,
                },
                whitespace: Whitespace::Undefined,
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum NumberBase {
    Binary,      // 0b
    Octal,       // 0o
    Decimal,     // 0d
    Hexadecimal, // 0x
}
#[derive(Clone, Debug, PartialEq)]
pub enum LiteralKind {
    Integer { content: String, base: NumberBase },
    Floating { content: String },
    Boolean(bool),
    //Char(char)
    String(String),
    //Unit,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}
#[derive(Clone, Copy, PartialEq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}
impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "Span {{{}:{}-{}:{}}}",
            self.start.line, self.start.column, self.end.line, self.end.column
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
    /// Creates the whitespace from given tokens. Those tokens are wrapped in an `Option`, to make handling first
    /// and last tokens easier for the caller
    pub fn from_token_kinds(
        left_token_kind: Option<&TokenKind>,
        right_token_kind: Option<&TokenKind>,
    ) -> Self {
        let is_prev_whitespace = left_token_kind.map(|tok| tok.is_to_skip()).unwrap_or(false);
        let is_next_whitespace = right_token_kind
            .map(|tok| tok.is_to_skip())
            .unwrap_or(false);

        Whitespace::from((is_prev_whitespace, is_next_whitespace))
    }
    // pub fn as_bools(self) -> (bool, bool) {
    //     use Whitespace::*;
    //     match self {
    //         None => (false, false),
    //         Left => (true, false),
    //         Right => (false, true),
    //         Both => (true, true),
    //         Undefined => unreachable!(),
    //     }
    // }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub whitespace: Whitespace,
}
