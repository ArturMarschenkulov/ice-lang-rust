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

//struct Position {
//    line: u32,
//    column: u32,
//}
//struct Span {
//    start: Position,
//    end: Position,
//}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Token {
    pub kind: TokenKind,
    //span: Span,
}
