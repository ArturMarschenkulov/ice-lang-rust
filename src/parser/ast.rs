//! The Ice abstract syntax tree module.
//!
//! This module contains types that form the language AST.
//!
//! The AST is the representation of the source code in a tree-like structure.
//! 
//! [`Identifier`], [`Operator`] and [`Literal`] represent the terminal nodes of the tree.


enum Terminal {
    Identifier(Identifier),
    Operator(Operator),
    Literal(Literal),
}

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
    type Error = String;
    fn try_from(token: crate::lexer::token::Token) -> Result<Self, Self::Error> {
        if token.kind.is_identifier() {
            Ok(Self { name: token })
        } else {
            Err(format!("Expected identifier, got {:?}", token.kind))
        }
    }
}

#[derive(Clone, Debug)]
pub struct Operator {
    pub name: crate::lexer::token::Token,
}

impl TryFrom<crate::lexer::token::Token> for Operator {
    type Error = String;

    fn try_from(token: crate::lexer::token::Token) -> Result<Self, Self::Error> {
        if token.kind.can_be_operator() {
            Ok(Self { name: token })
        } else {
            Err(format!("Expected operator, got {:?}", token.kind))
        }
    }
}

#[derive(Clone, Debug)]
pub struct Literal {
    pub kind: crate::lexer::token::LiteralKind,
}

impl From<crate::lexer::token::LiteralKind> for Literal {
    fn from(kind: crate::lexer::token::LiteralKind) -> Self {
        Self { kind }
    }
}

/// Represents a symbol in the AST.
/// 
/// A symbol is a path to a variable, function, struct, enum, etc.
/// If the path is empty, it means that the symbol is in the current scope (e.g., `x`).
/// If the path is not empty, it means that the symbol is in a different scope (e.g., `x::y::z`).
#[derive(Clone, Debug)]
pub struct Symbol {
    pub name: Identifier,
    /// `None` means there is no path to this symbol (`x`)
    /// `Some` means there is a path to this symbol (`x::y::z`),
    /// however it can be also empty (`::x`)
    pub path: Option<Vec<Identifier>>,
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
struct TypedExpr {
    expr: Expr,
    ty: Ty,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    /// E.g., 3, 5.0, "hello", 'c', true, false
    Literal(Literal),
    /// E.g., x, x::y::z, ::x
    Symbol(Symbol),
    FnCall(Box<Expr>, Vec<Expr>),
    /// E.g., (3, 5, 6)
    Grouping(Box<Expr>),

    /// E.g., 3 + 5, 3 * 5, 3 / 5, 3 - 5
    ///
    /// Note, that this will correctly evaluated only in the semantic analysis part.               
    BinaryInfix(Box<Expr>, Operator, Box<Expr>),
    /// E.g., -3, !true
    ///
    /// Note, that this will correctly evaluated only in the semantic analysis part.  
    UnaryPrefix(Operator, Box<Expr>),
    /// E.g., 3!
    ///
    /// Note, that this will correctly evaluated only in the semantic analysis part.
    UnaryPostfix(Box<Expr>, Operator),

    // E.g., { stmt; stmt; stmt; }
    Block(Vec<Stmt>),

    /// E.g., if expr { stmt; stmt; stmt; } else { stmt; stmt; stmt; }
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    While(Box<Expr>, Box<Expr>),
    For(Box<Stmt>, Box<Expr>, Box<Expr>, Box<Expr>),
    // Ret(Option<Box<Expr>>),
}
#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    // pub span: Span,
}
/// Impl block for constructors of `Expr`.
impl Expr {
    pub fn literal(literal: Literal) -> Self {
        Expr {
            kind: ExprKind::Literal(literal),
        }
    }
    pub fn group(expr: Expr) -> Self {
        Expr {
            kind: ExprKind::Grouping(Box::new(expr)),
        }
    }
    pub fn if_(expr: Expr, then: Expr, else_: Option<Expr>) -> Self {
        Expr {
            kind: ExprKind::If(Box::new(expr), Box::new(then), else_.map(Box::new)),
        }
    }
    pub fn while_(expr: Expr, body: Expr) -> Self {
        Expr {
            kind: ExprKind::While(Box::new(expr), Box::new(body)),
        }
    }
    pub fn for_(init: Stmt, cond: Expr, update: Expr, body: Expr) -> Self {
        Expr {
            kind: ExprKind::For(
                Box::new(init),
                Box::new(cond),
                Box::new(update),
                Box::new(body),
            ),
        }
    }
    pub fn block(stmts: Vec<Stmt>) -> Self {
        Expr {
            kind: ExprKind::Block(stmts),
        }
    }

    pub fn unary_prefix(op: Operator, expr: Expr) -> Self {
        Expr {
            kind: ExprKind::UnaryPrefix(op, Box::new(expr)),
        }
    }
    pub fn binary_infix(lhs: Expr, op: Operator, rhs: Expr) -> Self {
        Expr {
            kind: ExprKind::BinaryInfix(Box::new(lhs), op, Box::new(rhs)),
        }
    }

    pub fn symbol(name: Identifier, path: Option<Vec<Identifier>>) -> Self {
        Expr {
            kind: ExprKind::Symbol(Symbol { name, path }),
        }
    }

    pub fn fn_call(expr: Expr, args: Vec<Expr>) -> Self {
        Expr {
            kind: ExprKind::FnCall(Box::new(expr), args),
        }
    }
}

impl Expr {
    pub fn does_require_semicolon_if_in_stmt(&self) -> bool {
        !matches!(
            &self.kind,
            ExprKind::Block(..) | ExprKind::If(..) | ExprKind::While(..) | ExprKind::For(..)
        )
    }
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
impl From<Vec<Module>> for Project {
    fn from(module: Vec<Module>) -> Self {
        Self { modules: module }
    }
}
pub struct Module {
    pub items: Vec<Item>,
}
impl From<Vec<Item>> for Module {
    fn from(item: Vec<Item>) -> Self {
        Self { items: item }
    }
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
                ty if ["i", "u", "f"].iter().any(|x| ty.starts_with(x)) => true,
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

impl Ty {
    pub fn unit() -> Self {
        Self {
            kind: TyKind::Tuple(vec![]),
        }
    }
    pub fn simple(name: Identifier) -> Self {
        Self {
            kind: TyKind::Simple(name),
        }
    }
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

impl Item {
    pub fn fn_(name: Identifier, params: Vec<Parameter>, ret: Option<Ty>, body: Expr) -> Self {
        Item {
            kind: ItemKind::Fn {
                name,
                params,
                ret,
                body: Box::new(body),
            },
        }
    }
    pub fn struct_(name: Identifier, fields: Vec<Field>) -> Self {
        Item {
            kind: ItemKind::Struct { name, fields },
        }
    }
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

impl Stmt {
    pub fn noop() -> Self {
        Self {
            kind: StmtKind::NoOperation,
        }
    }
    pub fn item(item: Item) -> Self {
        Self {
            kind: StmtKind::Item(item),
        }
    }

    pub fn expr(expr: Expr) -> Self {
        Self {
            kind: StmtKind::Expression(Box::new(expr)),
        }
    }
    pub fn expr_without_semicolon(expr: Expr) -> Self {
        Self {
            kind: StmtKind::ExpressionWithoutSemicolon(Box::new(expr)),
        }
    }

    pub fn var(var: Identifier, ty: Option<Ty>, init: Option<Box<Expr>>) -> Self {
        Self {
            kind: StmtKind::Var { var, ty, init },
        }
    }
}
