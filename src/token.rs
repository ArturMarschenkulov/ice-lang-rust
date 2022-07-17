#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Punctuator(PunctuatorKind),
    Literal(LiteralKind),
    Identifier(String),
    Keyword(KeywordKind),
    SpecialKeyword(SpecialKeywordKind),
}

impl TokenKind {
    pub fn from_char(c: char) -> Self {
        use KeywordKind::*;
        use LiteralKind::*;
        use PunctuatorKind::*;
        use SpecialKeywordKind::*;
        use TokenKind::*;

        match c {
            '+' => Punctuator(Plus),
            '-' => Punctuator(Minus),
            '*' => Punctuator(Asterisk),
            '/' => Punctuator(Slash),
            '%' => Punctuator(Percent),
            '^' => Punctuator(Caret),
            '&' => Punctuator(Ampersand),
            '|' => Punctuator(VerticalBar),
            '!' => Punctuator(Exclamation),
            '=' => Punctuator(Equal),
            '<' => Punctuator(Less),
            '>' => Punctuator(Greater),
            '.' => Punctuator(Dot),
            ',' => Punctuator(Comma),
            ':' => Punctuator(Colon),
            ';' => Punctuator(Semicolon),
            '(' => Punctuator(LeftParen),
            ')' => Punctuator(RightParen),
            '[' => Punctuator(LeftBracket),
            ']' => Punctuator(RightBracket),
            '{' => Punctuator(LeftBrace),
            '}' => Punctuator(RightBrace),
            o => panic!(
                "This case is either invalid or not yet implemented: {:?}",
                o
            ),
        }
    }

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
                    | Asterisk
                    | Slash
                    | Equal
                    | Exclamation
                    | Greater
                    | Less
                    | Ampersand
                    | VerticalBar
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
                [Exclamation, Equal] => Punctuator(BangEqual),
                [Greater, Equal] => Punctuator(GreaterEqual),
                [Less, Equal] => Punctuator(LessEqual),
                [Ampersand, Ampersand] => Punctuator(AmpersandAmpersand),
                [VerticalBar, VerticalBar] => Punctuator(PipePipe),

                [Minus, Greater] => Punctuator(MinusGreater),

                [Plus, Equal] => Punctuator(PlusEqual),
                [Minus, Equal] => Punctuator(MinusEqual),
                [Asterisk, Equal] => Punctuator(StarEqual),
                [Slash, Equal] => Punctuator(SlashEqual),
                _ => self.clone(),
            },
            _ => self.clone(),
        }
    }
    pub fn complexify(self) -> TokenKind {
        use PunctuatorKind::*;
        use TokenKind::*;
        let tok = match &self {
            Punctuator(EqualEqual) => Punctuator(Complex(vec![Equal, Equal])),
            Punctuator(BangEqual) => Punctuator(Complex(vec![Exclamation, Equal])),
            Punctuator(GreaterEqual) => Punctuator(Complex(vec![Greater, Equal])),
            Punctuator(LessEqual) => Punctuator(Complex(vec![Less, Equal])),
            Punctuator(AmpersandAmpersand) => Punctuator(Complex(vec![Ampersand, Ampersand])),
            Punctuator(PipePipe) => Punctuator(Complex(vec![VerticalBar, VerticalBar])),
            Punctuator(MinusGreater) => Punctuator(Complex(vec![Minus, Greater])),
            Punctuator(PlusEqual) => Punctuator(Complex(vec![Plus, Equal])),
            Punctuator(MinusEqual) => Punctuator(Complex(vec![Minus, Equal])),
            Punctuator(StarEqual) => Punctuator(Complex(vec![Asterisk, Equal])),
            Punctuator(SlashEqual) => Punctuator(Complex(vec![Slash, Equal])),
            _ => self.clone(),
        };
        assert!(tok.clone().simplify() == self);
        tok
    }
    pub fn is_complex(&self) -> bool {
        use PunctuatorKind::*;
        use TokenKind::*;
        matches!(self.clone().complexify(), Punctuator(Complex(_)))
    }
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
    Colon,       // :
    Semicolon,   // ;
    Percent,     // %
    Dollar,      // $
    Question,    // ?
    Hash,        // #
    Dot,         // .
    Comma,       // ,
    Backslash,   // \
    At,          // @
    Caret,       // ^

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
impl PunctuatorKind {
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
                Complex(complex) => {
                    complex.iter().map(|t| t.as_str()).collect::<String>()
                }
                _ => unreachable!(),
            }
        } else {
            s.to_string()
        }

    }
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
    fn as_str(&self) -> &str {
        use KeywordKind::*;
        match self {
            Var => "var",
            Fn => "fn",

            If => "if",
            Else => "else",
            While => "while",
            For => "for",

            True => "true",
            False => "false",

            Print => "print",
            Println => "println",
        }
    }
    pub fn len(&self) -> usize {
        self.as_str().len()
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum SpecialKeywordKind {
    Eof,
    Newline,
    Whitespace,
    Comment, //Unknown,
}

impl SpecialKeywordKind {
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
    pub fn is_left_whitespace(c: char) -> bool {
        c == ' '
    }
    pub fn is_right_whitespace(c: char) -> bool {
        c == ' '
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
