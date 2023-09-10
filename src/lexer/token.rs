//! This module contains the definition of the [`Token`] struct and its associated types.

use super::span::Span;

use KeywordKind as KK;
use PunctuatorKind as PK;
use SpecialKeywordKind as SKK;
use TokenKind as TK;

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
        match &self {
            TK::Punctuator(p) => p.to_string(),
            TK::Literal(..) => todo!("it's not yet implemented for literals"), //l.as_str(),
            TK::Identifier(s) => s.to_string(),
            TK::Keyword(k) => <&str>::from(*k).to_owned(),
            TK::SpecialKeyword(k) => k.as_str().to_owned(),
        }
    }
    fn is_delimiter(&self) -> bool {
        match &self {
            TK::Punctuator(p) => p.is_delimiter(),
            _ => false,
        }
    }
    fn is_simple(&self) -> bool {
        match self {
            TK::Punctuator(p) => p.is_simple(),
            TK::SpecialKeyword(SKK::Eof)
            | TK::SpecialKeyword(SKK::Newline)
            | TK::SpecialKeyword(SKK::Whitespace) => true,
            _ => false,
        }
    }
    pub fn is_punctuator(&self) -> bool {
        matches!(self, TK::Punctuator(_))
    }
    pub fn can_be_operator(&self) -> bool {
        match self {
            TK::Punctuator(p) => p.can_be_operator(),
            _ => false,
        }
    }
    pub fn is_identifier(&self) -> bool {
        matches!(self, TK::Identifier(_))
    }
    pub fn starts_item(&self) -> bool {
        matches!(self, TK::Keyword(KK::Fn) | TK::Keyword(KK::Type))
    }
    // pub fn simplify(self) -> TokenKind {
    //     match self {
    //         TK::Punctuator(complex) => TK::Punctuator(complex.fuse()),
    //         _ => self.clone(),
    //     }
    // }
    // pub fn complexify(self) -> TokenKind {
    //     let tok = match self {
    //         TK::Punctuator(punc) => TK::Punctuator(punc.unfuse()),
    //         _ => self,
    //     };
    //     //assert!(tok.clone().simplify() == self.clone());
    //     tok
    // }
    // pub fn is_complex(&self) -> bool {
    //     matches!(self.clone().complexify(), TK::Punctuator(PK::Complex(_)))
    // }
    pub fn is_to_skip(&self) -> bool {
        matches!(
            self,
            TK::SpecialKeyword(SKK::Whitespace)
                | TK::SpecialKeyword(SKK::Newline)
                | TK::SpecialKeyword(SKK::Comment(..))
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
        match &self {
            PK::Complex(c) => match *c.as_slice() {
                [PK::Equal, PK::Equal] => PK::EqualEqual,
                [PK::Exclamation, PK::Equal] => PK::BangEqual,
                [PK::Greater, PK::Equal] => PK::GreaterEqual,
                [PK::Less, PK::Equal] => PK::LessEqual,
                [PK::Ampersand, PK::Ampersand] => PK::AmpersandAmpersand,
                [PK::VerticalBar, PK::VerticalBar] => PK::PipePipe,

                [PK::Minus, PK::Greater] => PK::MinusGreater,
                [PK::Colon, PK::Colon] => PK::ColonColon,

                [PK::Plus, PK::Equal] => PK::PlusEqual,
                [PK::Minus, PK::Equal] => PK::MinusEqual,
                [PK::Asterisk, PK::Equal] => PK::StarEqual,
                [PK::Slash, PK::Equal] => PK::SlashEqual,
                _ => self,
            },
            _ => self,
        }
    }
    // 'unfuses' fused complex tokens into simple complex tokens, if possible. Otherwise it does nothing.
    pub fn unfuse(self) -> PunctuatorKind {
        match self {
            PK::EqualEqual => PK::Complex(vec![PK::Equal, PK::Equal]),
            PK::BangEqual => PK::Complex(vec![PK::Exclamation, PK::Equal]),
            PK::GreaterEqual => PK::Complex(vec![PK::Greater, PK::Equal]),
            PK::LessEqual => PK::Complex(vec![PK::Less, PK::Equal]),
            PK::AmpersandAmpersand => PK::Complex(vec![PK::Ampersand, PK::Ampersand]),
            PK::PipePipe => PK::Complex(vec![PK::VerticalBar, PK::VerticalBar]),

            PK::MinusGreater => PK::Complex(vec![PK::Minus, PK::Greater]),
            PK::ColonColon => PK::Complex(vec![PK::Colon, PK::Colon]),

            PK::PlusEqual => PK::Complex(vec![PK::Plus, PK::Equal]),
            PK::MinusEqual => PK::Complex(vec![PK::Minus, PK::Equal]),
            PK::StarEqual => PK::Complex(vec![PK::Asterisk, PK::Equal]),
            PK::SlashEqual => PK::Complex(vec![PK::Slash, PK::Equal]),
            _ => self,
        }
    }
    fn is_simple(&self) -> bool {
        !matches!(self.clone().unfuse(), PK::Complex(_))
    }
    fn is_complex(&self) -> bool {
        !self.is_simple()
    }
    fn is_complex_fused(&self) -> bool {
        match self.clone() {
            PK::Complex(_) => false,
            token => !token.unfuse().is_simple(),
        }
    }

    fn is_delimiter(&self) -> bool {
        self.is_delimiter_open() || self.is_delimiter_close()
    }
    fn is_delimiter_open(&self) -> bool {
        matches!(self, PK::LeftParen | PK::LeftBracket | PK::LeftBrace)
    }
    fn is_delimiter_close(&self) -> bool {
        matches!(self, PK::RightParen | PK::RightBracket | PK::RightBrace)
    }

    /// Returns `true`, if the token is a structural token.
    ///
    /// Those are tokens which are integral for the structure of the language.
    /// Because of that the parser treats them in a more special way.
    ///
    /// Right now those tokens are `.`, `,`, `;`, `:`, `->`, `::`, `=>`.
    pub fn is_structural(&self) -> bool {
        match self {
            PK::Dot | PK::Comma | PK::Semicolon | PK::Colon | PK::Equal => true,
            PK::RightParen | PK::RightBracket | PK::RightBrace => true,
            PK::LeftParen | PK::LeftBracket | PK::LeftBrace => true,
            PK::MinusGreater | PK::ColonColon => true,
            PK::Complex(complex) => {
                matches!(
                    *complex.as_slice(),
                    [PK::Minus, PK::Greater] | [PK::Equal, PK::Greater]
                )
            }
            _ => false,
        }
    }
    fn can_be_operator(&self) -> bool {
        // ':', ';', ',' are 'structural' opereators, but they are not operators in the sense of the language.
        // The complex punctuators '->' and '=>' may also possibly become 'structural' operators, and thus
        // they should not be regarded as operators.
        match self {
            PK::Colon | PK::Semicolon | PK::Comma => false,
            token if token.is_complex_fused() => {
                !matches!(token, PK::MinusGreater /*| PK::EqualGreater*/)
            }
            PK::Complex(c) => !matches!(
                *c.as_slice(),
                [PK::Minus, PK::Greater] | [PK::Equal, PK::Greater]
            ),
            _ => true,
        }
    }
    fn is_overloadable(&self) -> bool {
        !matches!(self, PK::Colon | PK::Semicolon | PK::Comma | PK::Dot)
    }
    fn can_be_custom_op_part(&self) -> bool {
        match self {
            delim if delim.is_delimiter() => false,
            PK::Semicolon | PK::Comma | PK::Colon => false,
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
        Ok(match c {
            '+' => PK::Plus,
            '-' => PK::Minus,
            '*' => PK::Asterisk,
            '/' => PK::Slash,
            '=' => PK::Equal,
            '!' => PK::Exclamation,
            '>' => PK::Greater,
            '<' => PK::Less,
            '&' => PK::Ampersand,
            '|' => PK::VerticalBar,
            '%' => PK::Percent,
            '$' => PK::Dollar,
            '?' => PK::Question,
            '#' => PK::Hash,
            '\\' => PK::Backslash,
            '@' => PK::At,
            '^' => PK::Caret,
            '.' => PK::Dot,
            ':' => PK::Colon,
            ';' => PK::Semicolon,
            ',' => PK::Comma,
            '~' => PK::Tilde,
            '`' => PK::Backtick,

            '(' => PK::LeftParen,
            ')' => PK::RightParen,
            '[' => PK::LeftBracket,
            ']' => PK::RightBracket,
            '{' => PK::LeftBrace,
            '}' => PK::RightBrace,

            c => return Err(format!("{} is not a simple punctuator", c)),
        })
    }
}

impl From<PunctuatorKind> for String {
    fn from(kind: PunctuatorKind) -> Self {
        match kind {
            PK::Plus => "+",
            PK::Minus => "-",
            PK::Asterisk => "*",
            PK::Slash => "/",
            PK::Equal => "=",
            PK::Exclamation => "!",
            PK::Greater => ">",
            PK::Less => "<",
            PK::Ampersand => "&",
            PK::VerticalBar => "|",
            PK::Colon => ":",
            PK::Semicolon => ";",
            PK::Percent => "%",
            PK::Dollar => "$",
            PK::Question => "?",
            PK::Hash => "#",
            PK::Dot => ".",
            PK::Comma => ",",
            PK::Backslash => "\\",
            PK::At => "@",
            PK::Caret => "^",
            PK::Tilde => "~",
            PK::Backtick => "`",

            PK::LeftParen => "(",
            PK::RightParen => ")",
            PK::LeftBracket => "[",
            PK::RightBracket => "]",
            PK::LeftBrace => "{",
            PK::RightBrace => "}",

            PK::EqualEqual => "==",
            PK::BangEqual => "!=",
            PK::MinusGreater => "->",
            PK::AmpersandAmpersand => "&&",
            PK::PipePipe => "||",
            PK::GreaterEqual => ">=",
            PK::LessEqual => "<=",
            PK::PlusEqual => "+=",
            PK::MinusEqual => "-=",
            PK::StarEqual => "*=",
            PK::SlashEqual => "/=",
            PK::ColonColon => "::",

            PK::Complex(complex) => {
                return complex.iter().map(|t| t.to_string()).collect::<String>()
            }
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
        Ok(match s {
            "var" => KK::Var,
            "fn" => KK::Fn,
            "type" => KK::Type,
            "struct" => KK::Struct,

            "if" => KK::If,
            "else" => KK::Else,
            "while" => KK::While,
            "for" => KK::For,
            //"ret" => KK::Ret,
            ident => return Err(ident.to_owned()),
        })
    }
}

impl From<KeywordKind> for &str {
    fn from(keyword: KeywordKind) -> Self {
        match keyword {
            KK::Var => "var",
            KK::Fn => "fn",
            KK::Type => "type",
            KK::Struct => "struct",

            KK::If => "if",
            KK::Else => "else",
            KK::While => "while",
            KK::For => "for",
            //KK::Ret => "ret",
        }
    }
}

impl std::fmt::Display for KeywordKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            KK::Var => "'var'",
            KK::Fn => "'fn'",
            KK::Type => "'type'",
            KK::Struct => "'struct'",

            KK::If => "'if'",
            KK::Else => "'else'",
            KK::While => "'while'",
            KK::For => "'for'",
            //KK::Ret => "ret",
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
        match self {
            SKK::Eof => "eof",
            SKK::Newline => "newline",
            SKK::Whitespace => "whitespace",
            SKK::Comment(..) => "comment",
        }
    }
}

impl TryFrom<char> for SpecialKeywordKind {
    type Error = String;
    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            '\0' => Ok(SKK::Eof),
            '\n' => Ok(SKK::Newline),
            ' ' | '\t' | '\r' => Ok(SKK::Whitespace),
            _ => Err(c.to_string()),
        }
    }
}

/// Tokens a slice of tokens and converts them into a complex token.
///
/// Certain complex tokens are converted into a single token, which significes several tokens.
/// If token slice has only one token, the singular token itself is returned.
pub fn cook_tokens(tokens: &[Token]) -> Token {
    let tok_kinds = tokens
        .iter()
        .map(|token| {
            if let TK::Punctuator(kind) = token.kind.clone() {
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
            let token_kind = TK::Punctuator(PK::Complex(tok_kinds).fuse());

            let first_tok = tokens.first().unwrap();
            let last_tok = tokens.last().unwrap();
            let bools = (
                <(bool, bool)>::from(first_tok.whitespace).0,
                <(bool, bool)>::from(last_tok.whitespace).1,
            );
            Token {
                kind: token_kind,
                span: Span::from((first_tok.span, last_tok.span)),
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

impl From<(Whitespace, Whitespace)> for Whitespace {
    fn from(ws: (Whitespace, Whitespace)) -> Self {
        let left = ws.0;
        let right = ws.1;
        let left = <(bool, bool)>::from(left);
        let right = <(bool, bool)>::from(right);
        Whitespace::from((left.0, right.1))
    }
}

impl From<(char, char)> for Whitespace {
    fn from(chars: (char, char)) -> Self {
        let is_left = is_left_whitespace(&chars.0);
        let is_right = is_right_whitespace(&chars.1);
        Whitespace::from((is_left, is_right))
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
        // TODO: Maybe consider to move this logic somewhere else, where it is guaranteed
        // that the kind is a punctuator. Since guaranteeing it at runtime is expensive.
        let _ = slice
            .iter()
            .map(|t| assert!(t.kind.is_punctuator()))
            .collect::<Vec<_>>();

        let toks = slice
            .iter()
            .map(|t| {
                if let TK::Punctuator(punc) = t.kind.clone() {
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
            kind: TK::Punctuator(c),
            span: Span::from((left.span, right.span)),
            whitespace: Whitespace::from((left.whitespace, right.whitespace)),
        }
    }
}
