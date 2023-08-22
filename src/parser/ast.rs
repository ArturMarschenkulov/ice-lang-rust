#[derive(Clone, Debug)]
pub struct Identifier {
    pub name: crate::lexer::token::Token,
}

impl Identifier {
    pub fn name(&self) -> &str {
        if let crate::lexer::token::TokenKind::Identifier(name) = &self.name.kind {
            name
        } else {
            panic!("Expected identifier")
        }
    }
}

impl TryFrom<crate::lexer::token::Token> for Identifier {
    type Error = &'static str;
    fn try_from(token: crate::lexer::token::Token) -> Result<Self, Self::Error> {
        // // assert!(token.kind.is_identifier());
        // Identifier { name: token }
        Some(Identifier { name: token }).ok_or("Expected identifier")
    }
}

#[derive(Clone, Debug)]
pub struct Operator {
    pub name: crate::lexer::token::Token,
}

impl TryFrom<crate::lexer::token::Token> for Operator {
    type Error = &'static str;

    fn try_from(token: crate::lexer::token::Token) -> Result<Self, Self::Error> {
        Some(Operator { name: token }).ok_or("Expected operator")
    }
}

// enum Delimiter {
//     Parenthesis, // ( )
//     Brace,       // { }
//     Bracket,     // [ ]
// }

struct Typed<T> {
    expr: T,
    ty: Ty,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    /// E.g., 3, 5.0, "hello", 'c', true, false
    Literal(crate::lexer::token::LiteralKind),
    /// E.g., (3, 5, 6)
    Grouping(Box<Expr>),
    /// E.g., 3 + 5, 3 * 5, 3 / 5, 3 - 5                   
    BinaryInfix(Box<Expr>, Operator, Box<Expr>),
    /// E.g., -3, !true
    UnaryPrefix(Operator, Box<Expr>),
    Symbol {
        name: Identifier,
        /// `None` means there is no path to this symbol (`x`)
        /// `Some` means there is a path to this symbol (`x::y::z`),
        /// however it can be also empty (`::x`)
        path: Option<Vec<Identifier>>,
    }, // x, x::y::z, ::x
    // E.g., { stmt; stmt; stmt; }
    Block(Vec<Stmt>),

    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    While(Box<Expr>, Box<Expr>),
    For(Box<Stmt>, Box<Expr>, Box<Expr>, Box<Expr>),
    // Ret(Option<Box<Expr>>),
    FnCall(Box<Expr>, Vec<Expr>),
}
#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    // pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Parameter {
    pub name: Identifier,
    pub ty: Ty,
}

// NOTE: This is a placeholder name, since right now, we can only parse one file, so a whole project will always be a file.
// Later on, one should change the name, maybe 'Crate', 'Project', 'Package', 'Program', 'Module' etc.
// In Rust terms: `Project` is a `Crate` and `Module` is a `Module`.
pub struct Project {
    pub modules: Vec<Module>,
}
pub struct Module {
    pub items: Vec<Item>,
}
#[derive(Clone, Debug)]
pub enum TyKind {
    /// A simple type `A`. It can be either custom or builtin/primitive.
    ///
    /// Some builtin types are `i32`, `f32`, `bool`, `char`, `str`, `()`. They usually start with a small letter.
    ///
    /// Custom types are usually written in `PascalCase` and start with a capital letter.
    Simple(Identifier),
    /// A tuple type `(A, B, C)`. If empty it also represents the unit type `()`.
    Tuple(Vec<Ty>),
}
impl TyKind {
    /// Returns `true`, if the type is a buildin type.
    ///
    /// This includes `i32`, `f32`, `bool`, `char`, `str`, `()`.
    pub fn is_buildin(&self) -> bool {
        match self.name() {
            Some(ty) => match ty {
                ty if ty == "bool" => true,
                ty if ["i", "u", "f"].contains(&ty.get(..0).unwrap()) => true,
                ty if ty == "()" => true,
                _ => false,
            },
            _ => false,
        }
    }
    pub fn is_unit(&self) -> bool {
        match self {
            TyKind::Tuple(tys) => tys.is_empty(),
            _ => false,
        }
    }
    /// Returns `true`, if the type is a tuple type.
    pub fn is_tuple(&self) -> bool {
        // NOTE: This assumes that there are no 1-tuples. If there will be this will have to be changed.
        matches!(self, TyKind::Tuple(_))
    }
    /// Returns `true`, if the type is a simple type.
    pub fn is_simple(&self) -> bool {
        matches!(self, TyKind::Simple(_))
    }
    /// Returns the name of the type if it's a simple type.
    pub fn name(&self) -> Option<&str> {
        match self {
            TyKind::Simple(x) => Some(x.name()),
            _ if self.is_unit() => Some("()"),
            _ => None,
        }
    }
}
#[derive(Clone, Debug)]
pub struct Ty {
    pub kind: TyKind,
}
#[derive(Clone, Debug)]
pub struct Field {
    pub name: Identifier,
    pub ty: Ty,
}
#[derive(Clone, Debug)]
pub enum ItemKind {
    /// fn foo(x: i32) {...}
    Fn {
        name: Identifier,
        params: Vec<Parameter>,
        ret: Option<Ty>,
        body: Box<Expr>,
    },
    /// type Foo = struct { x: i32 }
    Struct {
        name: Identifier,
        fields: Vec<Field>,
    },
    // /// type Foo = enum { A, B }
    // Enum {
    //     name: Identifier,
    //     variants: Vec<Field>,
    // },

    // /// type Unit = {}
    // Unit {
    //     name: Identifier,
    // },
}
#[derive(Clone, Debug)]
pub struct Item {
    pub kind: ItemKind,
}

// The logic of the language and the way how it is parsed do not match.
// `ExpressionWithoutSemicolon` is in this a statement, however
// in the language itself it's an expression.
#[derive(Clone, Debug)]
pub enum StmtKind {
    /// var a: i32 = 4,
    Var {
        var: Identifier,
        ty: Option<Ty>,
        init: Option<Box<Expr>>,
    },
    /// Expression with a trailing semicolon
    Expression(Box<Expr>),
    /// Expression without a trailing semicolon
    ExpressionWithoutSemicolon(Box<Expr>),
    /// An item declaration/definition
    Item(Item),
    /// Only a trailing semicolon
    NoOperation,
}
#[derive(Clone, Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    // pub span: Span,
}