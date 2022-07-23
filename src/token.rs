#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Punctuator(PunctuatorKind),
    Literal(LiteralKind),
    Identifier(String),
    Keyword(KeywordKind),
    SpecialKeyword(SpecialKeywordKind),
}

impl TokenKind {
    pub fn as_str(&self) -> String {
        use TokenKind::*;
        match &self {
            Punctuator(p) => p.as_str(),
            Literal(l) => todo!("it's not yet implemented for literals"), //l.as_str(),
            Identifier(s) => s.to_string(),
            Keyword(k) => k.as_str().to_owned(),
            SpecialKeyword(k) => k.as_str().to_owned(),
        }
    }
    fn is_delimiter(&self) -> bool {
        use TokenKind::*;
        match &self {
            Punctuator(p) => p.is_delimiter(),
            _ => false,
        }
    }
    pub fn can_be_part_of_complex(&self) -> bool {
        use TokenKind::*;
        match self {
            Punctuator(p) => p.can_be_part_of_complex(),
            _ => false,
        }
    }
    pub fn is_simple(&self) -> bool {
        use SpecialKeywordKind::*;
        use TokenKind::*;
        match self {
            Punctuator(p) => p.is_simple(),
            SpecialKeyword(Eof) | SpecialKeyword(Newline) | SpecialKeyword(Whitespace) => true,
            _ => false,
        }
    }
    pub fn is_punctuator(&self) -> bool {
        matches!(self, TokenKind::Punctuator(_))
    }
    pub fn is_literal(&self) -> bool {
        matches!(self, TokenKind::Literal(_))
    }
    pub fn is_keyword(&self) -> bool {
        matches!(self, TokenKind::Keyword(_))
    }
    pub fn is_identifier(&self) -> bool {
        matches!(self, TokenKind::Identifier(_))
    }
    // pub fn simplify(self) -> TokenKind {
    //     use TokenKind::*;
    //     match self {
    //         Punctuator(complex) => Punctuator(complex.fuse()),
    //         _ => self.clone(),
    //     }
    // }
    // pub fn complexify(self) -> TokenKind {
    //     use PunctuatorKind::*;
    //     use TokenKind::*;
    //     let tok = match self {
    //         Punctuator(punc) => Punctuator(punc.unfuse()),
    //         _ => self,
    //     };
    //     //assert!(tok.clone().simplify() == self.clone());
    //     tok
    // }
    // pub fn is_complex(&self) -> bool {
    //     use PunctuatorKind::*;
    //     use TokenKind::*;
    //     matches!(self.clone().complexify(), Punctuator(Complex(_)))
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
    Plus,        // +
    Minus,       // -
    Asterisk,    // *
    Slash,       // /
    Equal,       // =
    Exclamation, // !
    Greater,     // >
    Less,        // <
    Ampersand,   // &
    VerticalBar, // |

    Percent,   // %
    Dollar,    // $
    Question,  // ?
    Hash,      // #
    Backslash, // \
    At,        // @
    Caret,     // ^
    Tilde,     // ~
    Backtick,  // `
    Dot,       // .

    Colon,     // :
    Semicolon, // ;
    Comma,     // ,

    LeftParen,    // (
    RightParen,   // )
    LeftBracket,  // [
    RightBracket, // ]
    LeftBrace,    // {
    RightBrace,   // }

    // Punctuators two
    // Below are the complex punctuators.
    Complex(Vec<PunctuatorKind>),

    // These are the 'fused complex punctuators'.
    // Basically complex punctuators, which get a special treatement, either for now or forever
    EqualEqual, // ==
    BangEqual,  // !=

    AmpersandAmpersand, // &&
    PipePipe,           // ||

    GreaterEqual, // >=
    LessEqual,    // <=

    PlusEqual,  // +=
    MinusEqual, // -=
    StarEqual,  // *=
    SlashEqual, // /=

    // These are complex structural tokens. `EqualGreater` might
    MinusGreater, // ->
    // EqualGreater,       // =>
    ColonColon, // ::
}

impl PunctuatorKind {
    pub fn from_char(c: char) -> Option<PunctuatorKind> {
        use PunctuatorKind::*;
        match c {
            '+' => Some(Plus),
            '-' => Some(Minus),
            '*' => Some(Asterisk),
            '/' => Some(Slash),
            '=' => Some(Equal),
            '!' => Some(Exclamation),
            '>' => Some(Greater),
            '<' => Some(Less),
            '&' => Some(Ampersand),
            '|' => Some(VerticalBar),
            '%' => Some(Percent),
            '$' => Some(Dollar),
            '?' => Some(Question),
            '#' => Some(Hash),
            '\\' => Some(Backslash),
            '@' => Some(At),
            '^' => Some(Caret),
            '.' => Some(Dot),
            ':' => Some(Colon),
            ';' => Some(Semicolon),
            ',' => Some(Comma),
            '~' => Some(Tilde),
            '`' => Some(Backtick),

            '(' => Some(LeftParen),
            ')' => Some(RightParen),
            '[' => Some(LeftBracket),
            ']' => Some(RightBracket),
            '{' => Some(LeftBrace),
            '}' => Some(RightBrace),
            _ => None,
        }
    }
    // 'fuses' simple complex tokens into fused complex tokens, if possible. Otherwise it does nothing.
    fn fuse(self) -> PunctuatorKind {
        use PunctuatorKind::*;
        match &self {
            Complex(c) => match *c.as_slice() {
                [Equal, Equal] => EqualEqual,
                [Exclamation, Equal] => BangEqual,
                [Greater, Equal] => GreaterEqual,
                [Less, Equal] => LessEqual,
                [Ampersand, Ampersand] => AmpersandAmpersand,
                [VerticalBar, VerticalBar] => PipePipe,

                [Minus, Greater] => MinusGreater,
                [Colon, Colon] => ColonColon,

                [Plus, Equal] => PlusEqual,
                [Minus, Equal] => MinusEqual,
                [Asterisk, Equal] => StarEqual,
                [Slash, Equal] => SlashEqual,
                _ => self,
            },
            _ => self,
        }
    }
    // 'unfuses' fused complex tokens into simple complex tokens, if possible. Otherwise it does nothing.
    pub fn unfuse(self) -> PunctuatorKind {
        use PunctuatorKind::*;
        match self {
            EqualEqual => Complex(vec![Equal, Equal]),
            BangEqual => Complex(vec![Exclamation, Equal]),
            GreaterEqual => Complex(vec![Greater, Equal]),
            LessEqual => Complex(vec![Less, Equal]),
            AmpersandAmpersand => Complex(vec![Ampersand, Ampersand]),
            PipePipe => Complex(vec![VerticalBar, VerticalBar]),

            MinusGreater => Complex(vec![Minus, Greater]),
            ColonColon => Complex(vec![Colon, Colon]),

            PlusEqual => Complex(vec![Plus, Equal]),
            MinusEqual => Complex(vec![Minus, Equal]),
            StarEqual => Complex(vec![Asterisk, Equal]),
            SlashEqual => Complex(vec![Slash, Equal]),
            _ => self,
        }
    }
    fn is_simple(&self) -> bool {
        use PunctuatorKind::*;
        !matches!(self.clone().unfuse(), Complex(_))
    }
    fn is_complex(&self) -> bool {
        !self.is_simple()
    }
    fn is_complex_fused(&self) -> bool {
        use PunctuatorKind::*;
        match self.clone() {
            Complex(_) => false,
            token => !token.unfuse().is_simple(),
        }
    }

    fn is_delimiter(&self) -> bool {
        use PunctuatorKind::*;
        matches!(
            self,
            LeftParen | RightParen | LeftBracket | RightBracket | LeftBrace | RightBrace
        )
    }
    fn is_delimiter_open(&self) -> bool {
        use PunctuatorKind::*;
        matches!(self, LeftParen | LeftBracket | LeftBrace)
    }
    fn is_delimiter_close(&self) -> bool {
        use PunctuatorKind::*;
        matches!(self, RightParen | RightBracket | RightBrace)
    }

    /// Returns `true`, if the token is a structural token.
    /// Those are tokens which are integral for the structure of the language.
    /// Because of that the parser treats them in a more special way.
    ///
    /// Right now those tokens are `.`, `,`, `;`, `:`, `->`, `::`, `=>`.
    pub fn is_structural(&self) -> bool {
        use PunctuatorKind::*;
        match self {
            Dot | Comma | Semicolon | Colon | Equal=> true,
            RightParen | RightBracket | RightBrace => true,
            LeftParen | LeftBracket | LeftBrace => true,
            MinusGreater | ColonColon => true,
            Complex(complex) => matches!(*complex.as_slice(), [Minus, Greater] | [Equal, Greater]),
            _ => false,
        }
    }

    /// Returns `true`, if the token can be part of a complex token.
    ///
    /// The are special rules for structural tokens to be part of complex tokens.
    pub fn can_be_part_of_complex(&self) -> bool {
        use PunctuatorKind::*;

        match self {
            p => !matches!(
                p,
                Semicolon
                    | Comma
                    | Dot
                    | Colon
                    // Delimiters can never be part of a complex expression.
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
    fn can_be_operator(&self) -> bool {
        use PunctuatorKind::*;
        // ':', ';', ',' are 'structural' opereators, but they are not operators in the sense of the language.
        // The complex punctuators '->' and '=>' may also possibly become 'structural' operators, and thus
        // they should not be regarded as operators.
        match self {
            Colon | Semicolon | Comma => false,
            token if token.is_complex_fused() => {
                !matches!(token, MinusGreater /*| EqualGreater*/)
            }
            Complex(c) => !matches!(*c.as_slice(), [Minus, Greater] | [Equal, Greater]),
            _ => true,
        }
    }
    fn is_overloadable(&self) -> bool {
        use PunctuatorKind::*;
        !matches!(self, Colon | Semicolon | Comma | Dot)
    }
    fn can_be_custom_op_part(&self) -> bool {
        use PunctuatorKind::*;
        match self {
            delim if delim.is_delimiter() => false,
            Semicolon | Comma | Colon => false,
            _ => true,
        }
    }

    fn as_str(&self) -> String {
        use PunctuatorKind::*;
        let s = match self {
            Plus => "+",
            Minus => "-",
            Asterisk => "*",
            Slash => "/",
            Equal => "=",
            Exclamation => "!",
            Greater => ">",
            Less => "<",
            Ampersand => "&",
            VerticalBar => "|",
            Colon => ":",
            Semicolon => ";",
            Percent => "%",
            Dollar => "$",
            Question => "?",
            Hash => "#",
            Dot => ".",
            Comma => ",",
            Backslash => "\\",
            At => "@",
            Caret => "^",

            LeftParen => "(",
            RightParen => ")",
            LeftBracket => "[",
            RightBracket => "]",
            LeftBrace => "{",
            RightBrace => "}",

            EqualEqual => "==",
            BangEqual => "!=",
            MinusGreater => "->",
            AmpersandAmpersand => "&&",
            PipePipe => "||",
            GreaterEqual => ">=",
            LessEqual => "<=",
            PlusEqual => "+=",
            MinusEqual => "-=",
            StarEqual => "*=",
            SlashEqual => "/=",
            _ => "",
        };
        if s.is_empty() {
            match self {
                Complex(complex) => complex.iter().map(|t| t.as_str()).collect::<String>(),
                _ => unreachable!(),
            }
        } else {
            s.to_string()
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum KeywordKind {
    Var,
    Fn,

    If,
    Else,
    While,
    For,

    Print,
    Println,
}
impl KeywordKind {
    fn as_str(&self) -> &str {
        use KeywordKind::*;
        match self {
            Var => "var",
            Fn => "fn",

            If => "if",
            Else => "else",
            While => "while",
            For => "for",

            Print => "print",
            Println => "println",
        }
    }
    pub fn len(&self) -> usize {
        self.as_str().len()
    }
}
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SpecialKeywordKind {
    Eof,
    Newline,
    Whitespace,
    Comment, //Unknown,
}

impl SpecialKeywordKind {
    pub fn from_char(c: char) -> Option<Self> {
        use SpecialKeywordKind::*;
        match c {
            '\0' => Some(Eof),
            '\n' => Some(Newline),
            ' ' | '\t' | '\r' => Some(Whitespace),
            _ => None,
        }
    }
    fn as_str(&self) -> &str {
        use SpecialKeywordKind::*;
        match self {
            Eof => "eof",
            Newline => "newline",
            Whitespace => "whitespace",
            Comment => "comment",
        }
    }
}

/// Tokens a slice of tokens and converts them into a complex token.
/// Certain complex tokens are converted into a single token, which significes several tokens.
/// If token slice has only one token, the singular token itself is returned.
pub fn cook_tokens(tokens: &[Token]) -> Token {
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
            let token_kind = Punctuator(Complex(tok_kinds).fuse());

            let first_tok = tokens.first().unwrap();
            let last_tok = tokens.last().unwrap();
            let bools = (
                first_tok.whitespace.as_bools().0,
                last_tok.whitespace.as_bools().1,
            );
            Token {
                kind: token_kind,
                span: Span {
                    start: first_tok.span.start,
                    end: last_tok.span.end,
                },
                whitespace: Whitespace::from(bools),
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
impl NumberBase {
    pub fn as_num(&self) -> u8 {
        use NumberBase::*;
        match self {
            Binary => 2,
            Octal => 8,
            Decimal => 10,
            Hexadecimal => 16,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralKind {
    Integer { content: String, base: NumberBase },
    Floating { content: String },
    Boolean(bool),
    Char(char),
    Str(String),
    //Unit,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}
impl Position {
    pub fn new(line: u32, column: u32) -> Position {
        Position { line, column }
    }
}
#[derive(Clone, Copy, PartialEq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}
impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Span { start, end }
    }
    pub fn from_tuples(start: (u32, u32), end: (u32, u32)) -> Self {
        Span {
            start: Position::new(start.0, start.1),
            end: Position::new(end.0, end.1),
        }
    }
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
}
impl Whitespace {
    pub fn from(bools: (bool, bool)) -> Self {
        use Whitespace::*;
        match bools {
            (true, true) => Both,
            (true, false) => Left,
            (false, true) => Right,
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
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub whitespace: Whitespace,
}
impl Token {
    pub fn new(kind: TokenKind, span: Span, whitespace: Whitespace) -> Self {
        Token {
            kind,
            span,
            whitespace,
        }
    }
}
