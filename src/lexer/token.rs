#[derive(Debug, PartialEq, Eq, Clone)]
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
            Punctuator(p) => p.to_string(),
            Literal(..) => todo!("it's not yet implemented for literals"), //l.as_str(),
            Identifier(s) => s.to_string(),
            Keyword(k) => <&str>::from(*k).to_owned(),
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
    fn is_simple(&self) -> bool {
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
    pub fn starts_item(&self) -> bool {
        use KeywordKind::*;
        use TokenKind::*;
        matches!(self, Keyword(Fn) | Keyword(Type))
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
            SpecialKeyword(Whitespace) | SpecialKeyword(Newline) | SpecialKeyword(Comment(..))
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
    // fn from_self_slice(slice: &[Self]) -> Self {
    //     assert!(slice.len() > 0, "PunctuatorKind::from_self_slice: slice must have at least length 1");

    //     let c = PunctuatorKind::Complex(slice.to_vec());
    // }
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
        self.is_delimiter_open() || self.is_delimiter_close()
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
    ///
    /// Those are tokens which are integral for the structure of the language.
    /// Because of that the parser treats them in a more special way.
    ///
    /// Right now those tokens are `.`, `,`, `;`, `:`, `->`, `::`, `=>`.
    pub fn is_structural(&self) -> bool {
        use PunctuatorKind::*;
        match self {
            Dot | Comma | Semicolon | Colon | Equal => true,
            RightParen | RightBracket | RightBrace => true,
            LeftParen | LeftBracket | LeftBrace => true,
            MinusGreater | ColonColon => true,
            Complex(complex) => matches!(*complex.as_slice(), [Minus, Greater] | [Equal, Greater]),
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

    // fn to_string(&self) -> String {
    //     format!("{}", self)
    // }
}

impl TryFrom<char> for PunctuatorKind {
    type Error = String;
    fn try_from(c: char) -> Result<Self, Self::Error> {
        use PunctuatorKind::*;
        Ok(match c {
            '+' => Plus,
            '-' => Minus,
            '*' => Asterisk,
            '/' => Slash,
            '=' => Equal,
            '!' => Exclamation,
            '>' => Greater,
            '<' => Less,
            '&' => Ampersand,
            '|' => VerticalBar,
            '%' => Percent,
            '$' => Dollar,
            '?' => Question,
            '#' => Hash,
            '\\' => Backslash,
            '@' => At,
            '^' => Caret,
            '.' => Dot,
            ':' => Colon,
            ';' => Semicolon,
            ',' => Comma,
            '~' => Tilde,
            '`' => Backtick,

            '(' => LeftParen,
            ')' => RightParen,
            '[' => LeftBracket,
            ']' => RightBracket,
            '{' => LeftBrace,
            '}' => RightBrace,

            c => return Err(format!("{} is not a simple punctuator", c)),
        })
    }
}

impl From<PunctuatorKind> for String {
    fn from(kind: PunctuatorKind) -> Self {
        use PunctuatorKind::*;
        match kind {
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
            Tilde => "~",
            Backtick => "`",

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
            ColonColon => "::",

            Complex(complex) => return complex.iter().map(|t| t.to_string()).collect::<String>(),
        }
        .to_owned()
    }
}

impl std::fmt::Display for PunctuatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", String::from(self.clone()))
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum KeywordKind {
    Var,
    Fn,
    Type,
    Struct, // This kind should be able to be context sensitive

    If,
    Else,
    While,
    For,
    // Ret,
}

impl KeywordKind {
    pub fn len(&self) -> usize {
        <&str>::from(*self).len()
    }
}

impl TryFrom<&str> for KeywordKind {
    type Error = String;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        use KeywordKind::*;
        Ok(match s {
            "var" => Var,
            "fn" => Fn,
            "type" => Type,
            "struct" => Struct,

            "if" => If,
            "else" => Else,
            "while" => While,
            "for" => For,
            //"ret" => Ret,
            ident => return Err(ident.to_owned()),
        })
    }
}

impl From<KeywordKind> for &str {
    fn from(keyword: KeywordKind) -> Self {
        use KeywordKind::*;
        match keyword {
            Var => "var",
            Fn => "fn",
            Type => "type",
            Struct => "struct",

            If => "if",
            Else => "else",
            While => "while",
            For => "for",
            //Ret => "ret",
        }
    }
}

impl std::fmt::Display for KeywordKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use KeywordKind::*;
        let s = match self {
            Var => "'var'",
            Fn => "'fn'",
            Type => "'type'",
            Struct => "'struct'",

            If => "'if'",
            Else => "'else'",
            While => "'while'",
            For => "'for'",
            //Ret => "ret",
        };
        write!(f, "{}", s)
    }
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CommentKind {
    Line(String),
    Block(String),
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SpecialKeywordKind {
    Eof,
    Newline,
    Whitespace,
    Comment(CommentKind),
}

impl SpecialKeywordKind {
    fn as_str(&self) -> &str {
        use SpecialKeywordKind::*;
        match self {
            Eof => "eof",
            Newline => "newline",
            Whitespace => "whitespace",
            Comment(..) => "comment",
        }
    }
}

impl TryFrom<char> for SpecialKeywordKind {
    type Error = String;
    fn try_from(c: char) -> Result<Self, Self::Error> {
        use SpecialKeywordKind::*;
        match c {
            '\0' => Ok(Eof),
            '\n' => Ok(Newline),
            ' ' | '\t' | '\r' => Ok(Whitespace),
            _ => Err(c.to_string()),
        }
    }
}

/// Tokens a slice of tokens and converts them into a complex token.
/// 
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
                <(bool, bool)>::from(first_tok.whitespace).0,
                <(bool, bool)>::from(last_tok.whitespace).1,
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NumberBase {
    Binary,      // 0b
    Octal,       // 0o
    Decimal,     // 0d
    Hexadecimal, // 0x
}
impl NumberBase {
    pub fn is_char_in(&self, c: char) -> bool {
        c.is_digit(u32::from(*self))
    }
}

impl TryFrom<u32> for NumberBase {
    type Error = String;
    fn try_from(s: u32) -> Result<Self, Self::Error> {
        use NumberBase::*;
        Ok(match s {
            2 => Binary,
            8 => Octal,
            10 => Decimal,
            16 => Hexadecimal,
            _ => return Err(s.to_string()),
        })
    }
}
impl TryFrom<&str> for NumberBase {
    type Error = String;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        use NumberBase::*;
        Ok(match s {
            "0b" => Binary,
            "0o" => Octal,
            "0d" => Decimal,
            "0x" => Hexadecimal,
            _ => return Err(s.to_owned()),
        })
    }
}
impl From<NumberBase> for u32 {
    fn from(base: NumberBase) -> Self {
        use NumberBase::*;
        match base {
            Binary => 2,
            Octal => 8,
            Decimal => 10,
            Hexadecimal => 16,
        }
    }
}
impl From<NumberBase> for &str {
    fn from(base: NumberBase) -> Self {
        use NumberBase::*;
        match base {
            Binary => "0b",
            Octal => "0o",
            Decimal => "0d",
            Hexadecimal => "0x",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Number {
    content: String,
    prefix: Option<NumberBase>,
    suffix: Option<String>,
    is_float: bool,
}

impl Number {
    pub fn new(
        content: &str,
        prefix: Option<NumberBase>,
        suffix: Option<String>,
        is_float: bool,
    ) -> Self {
        if is_float && prefix.is_some() {
            panic!("Floating point numbers cannot have a prefix");
        }
        Self {
            content: content.to_owned(),
            prefix,
            suffix,
            is_float,
        }
    }
    pub fn integer(content: &str, prefix: Option<NumberBase>, suffix: Option<String>) -> Self {
        Self {
            content: content.to_owned(),
            prefix,
            suffix,
            is_float: false,
        }
    }
    pub fn floating(content: &str, suffix: Option<String>) -> Self {
        Self {
            content: content.to_owned(),
            prefix: None,
            suffix,
            is_float: true,
        }
    }
}

impl From<Number> for String {
    fn from(number: Number) -> Self {
        let prefix = match number.prefix {
            Some(base) => <&str>::from(base),
            None => "",
        };
        let suffix = match number.suffix {
            Some(s) => s,
            None => "".to_owned(),
        };
        format!("{}{}{}", prefix, number.content, suffix)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LiteralKind {
    Number(Number),
    Boolean(bool),
    Char(char),
    Str(String),
    //Unit,
}
impl LiteralKind {
    pub fn integer(content: &str, prefix: Option<NumberBase>, suffix: Option<String>) -> Self {
        LiteralKind::Number(Number::integer(content, prefix, suffix))
    }
    pub fn floating(content: &str, suffix: Option<String>) -> Self {
        LiteralKind::Number(Number::floating(content, suffix))
    }
    pub fn boolean(content: bool) -> Self {
        LiteralKind::Boolean(content)
    }
}
impl TryFrom<&str> for LiteralKind {
    type Error = String;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s {
            "true" => Ok(Self::boolean(true)),
            "false" => Ok(Self::boolean(false)),
            ident => Err(ident.to_owned()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}
impl Position {
    pub fn new(line: u32, column: u32) -> Position {
        Position { line, column }
    }
}
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}
impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Span { start, end }
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
impl From<((u32, u32), (u32, u32))> for Span {
    fn from(tuples: ((u32, u32), (u32, u32))) -> Self {
        Span {
            start: Position::new(tuples.0 .0, tuples.0 .1),
            end: Position::new(tuples.1 .0, tuples.1 .1),
        }
    }
}

/// Represents the whitespace before and after a token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Whitespace {
    NoBoth,
    Left,
    Right,
    Both,
}

fn is_left_whitespace(c: &char) -> bool {
    c == &' '
}
fn is_right_whitespace(c: &char) -> bool {
    c == &' '
}

impl From<Whitespace> for (bool, bool) {
    fn from(ws: Whitespace) -> Self {
        use Whitespace::*;
        match ws {
            NoBoth => (false, false),
            Left => (true, false),
            Right => (false, true),
            Both => (true, true),
        }
    }
}

impl From<(Whitespace, Whitespace)> for Whitespace {
    fn from(ws: (Whitespace, Whitespace)) -> Self {
        let left = ws.0;
        let right = ws.1;
        Whitespace::from((<(bool, bool)>::from(left).0, <(bool, bool)>::from(right).1))
    }
}

impl From<(bool, bool)> for Whitespace {
    fn from(bools: (bool, bool)) -> Self {
        use Whitespace::*;
        match bools {
            (true, true) => Both,
            (true, false) => Left,
            (false, true) => Right,
            (false, false) => NoBoth,
        }
    }
}
impl From<(char, char)> for Whitespace {
    fn from(chars: (char, char)) -> Self {
        use Whitespace::*;
        match (is_left_whitespace(&chars.0), is_right_whitespace(&chars.1)) {
            (true, true) => Both,
            (true, false) => Left,
            (false, true) => Right,
            (false, false) => NoBoth,
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub fn from_self_slice(slice: &[Token]) -> Self {
        use TokenKind::*;
        // TODO: Maybe consider to move this logic somewhere else, where it is guaranteed
        // that the kind is a punctuator. Since guaranteeing it at runtime is expensive.
        let _ = slice
            .iter()
            .map(|t| assert!(t.kind.is_punctuator()))
            .collect::<Vec<_>>();

        let toks = slice
            .iter()
            .map(|t| {
                if let Punctuator(punc) = t.kind.clone() {
                    punc
                } else {
                    unreachable!()
                }
            })
            .collect::<Vec<_>>();

        let c = PunctuatorKind::Complex(toks);

        let left = slice.first().unwrap();
        let right = slice.last().unwrap();
        Token {
            kind: Punctuator(c),
            span: Span::new(left.span.start, right.span.end),
            whitespace: Whitespace::from((left.whitespace, right.whitespace)),
        }
    }
}
