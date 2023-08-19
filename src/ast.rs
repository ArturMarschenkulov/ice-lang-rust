#[derive(Clone, Debug)]
pub struct Identifier {
    pub name: crate::token::Token,
}

impl Identifier {
    fn name(&self) -> &str {
        if let crate::token::TokenKind::Identifier(name) = &self.name.kind {
            name
        } else {
            panic!("Expected identifier")
        }
    }
}

impl From<crate::token::Token> for Identifier {
    fn from(token: crate::token::Token) -> Self {
        // assert!(token.kind.is_identifier());
        Identifier { name: token }
    }
}

#[derive(Clone, Debug)]
pub struct Operator {
    pub name: crate::token::Token,
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
    Literal(crate::token::LiteralKind), // 3, 5.0, "hello", 'c', true, false
    Grouping(Box<Expr>),                // (3, 5, 6)
    BinaryInfix(Box<Expr>, Operator, Box<Expr>), // 3 + 5, 3 * 5, 3 / 5, 3 - 5
    UnaryPrefix(Operator, Box<Expr>), // -3, !true
    Symbol {
        name: Identifier,
        /// `None` means there is no path to this symbol (`x`)
        /// `Some` means there is a path to this symbol (`x::y::z`),
        /// however it can be also empty (`::x`)
        path: Option<Vec<Identifier>>,
    }, // x, x::y::z, ::x
    Block(Vec<Stmt>),                   // { stmt; stmt; stmt; }

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

pub trait DebugTreePrinter {
    fn print_debug_tree(&self);
}

impl<T> DebugTreePrinter for Vec<T>
where
    T: DebugTreePrinter,
{
    fn print_debug_tree(&self) {
        for tree_s in self {
            debug_tree::defer_print!();
            tree_s.print_debug_tree();
        }
    }
}

impl DebugTreePrinter for Project {
    fn print_debug_tree(&self) {
        debug_tree::defer_print!();
        debug_tree::add_branch!("Project: ");
        for module in &self.modules {
            module.print_debug_tree();
        }
    }
}
impl DebugTreePrinter for Module {
    fn print_debug_tree(&self) {
        debug_tree::defer_print!();
        debug_tree::add_branch!("Module: ");
        for item in &self.items {
            item.print_debug_tree();
        }
    }
}

impl DebugTreePrinter for Expr {
    fn print_debug_tree(&self) {
        match &self.kind {
            ExprKind::Literal(lit) => {
                debug_tree::add_branch!("Lit {:?}", lit);
            }
            ExprKind::Grouping(expr) => {
                debug_tree::add_branch!("Group: ");
                expr.print_debug_tree();
            }
            ExprKind::BinaryInfix(expr_l, op, expr_r) => {
                debug_tree::add_branch!("Binary: {:?}", op.name.kind);
                expr_l.print_debug_tree();
                expr_r.print_debug_tree();
            }
            ExprKind::UnaryPrefix(op, expr) => {
                debug_tree::add_branch!("Unary: {:?}", op.name.kind);
                expr.print_debug_tree();
            }
            ExprKind::Symbol { name, path } => {
                let s = if path.is_some() {
                    let s = path
                        .clone()
                        .unwrap()
                        .iter()
                        .map(|t| {
                            if let crate::token::TokenKind::Identifier(id) = &t.name.kind {
                                id.clone()
                            } else {
                                panic!("Expected identifier")
                            }
                        })
                        .collect::<Vec<String>>();
                    Some(s)
                } else {
                    None
                };
                debug_tree::add_branch!("Symbol: {}, {:?}", name.name.kind.as_str(), s);
            }
            ExprKind::Block(stmts) => {
                debug_tree::add_branch!("Block: ");
                for ast_s in stmts {
                    ast_s.print_debug_tree();
                }
            }
            ExprKind::If(comp, then_branch, else_branch) => {
                debug_tree::add_branch!("If: ");
                comp.print_debug_tree();
                then_branch.print_debug_tree();
                if let Some(block) = else_branch {
                    block.print_debug_tree();
                }
            }
            ExprKind::While(comp, body) => {
                debug_tree::add_branch!("While: ");
                comp.print_debug_tree();
                body.print_debug_tree();
            }
            ExprKind::For(init, comp, expr, body) => {
                debug_tree::add_branch!("For: ");
                init.print_debug_tree();
                comp.print_debug_tree();
                expr.print_debug_tree();
                body.print_debug_tree();
            }
            ExprKind::FnCall(callable, parameters) => {
                debug_tree::add_branch!("FnCall: ");
                callable.print_debug_tree();
                {
                    debug_tree::add_branch!("parameters: ");
                    for _be1 in parameters {
                        _be1.print_debug_tree();
                    }
                }
            }
        }
    }
}

impl DebugTreePrinter for Stmt {
    fn print_debug_tree(&self) {
        match &self.kind {
            StmtKind::Var { var, ty, init } => {
                debug_tree::add_branch!("SVarDeclaration: ");
                debug_tree::add_leaf!("T: {:?}", var.name.kind);
                debug_tree::add_leaf!("T: {:?}", ty);
                match init.as_ref() {
                    Some(i) => i.print_debug_tree(),
                    None => debug_tree::add_leaf!("T: {:?}", init),
                }
                // init.as_ref().unwrap().print();
            }
            StmtKind::Expression(expr) => {
                debug_tree::add_branch!("SExpression: ");
                expr.print_debug_tree();
            }
            StmtKind::ExpressionWithoutSemicolon(expr) => {
                debug_tree::add_branch!("SExpressionWithoutSemicolon: ");
                expr.print_debug_tree();
            }
            StmtKind::Item(item) => {
                item.print_debug_tree();
            }
            StmtKind::NoOperation => {
                debug_tree::add_branch!("SNoOp");
            }
        }
    }
}

impl DebugTreePrinter for Ty {
    fn print_debug_tree(&self) {
        match &self.kind {
            TyKind::Simple(s) => {
                debug_tree::add_branch!("Simple: {:?}", s.name.kind);
            }
            TyKind::Tuple(tup) => {
                debug_tree::add_branch!("Tuple: ");
                for ty in tup {
                    ty.print_debug_tree();
                }
            }
        }
    }
}
impl DebugTreePrinter for Item {
    fn print_debug_tree(&self) {
        match &self.kind {
            ItemKind::Fn {
                name,
                params,
                ret,
                body,
            } => {
                debug_tree::add_branch!("SFnDeclaration: ");
                debug_tree::add_leaf!("name: {:?}", name.name.kind);
                {
                    debug_tree::add_branch!("parameters: ");

                    for param in params {
                        param.ty.print_debug_tree();
                    }
                }
                {
                    debug_tree::add_branch!("return type: ");
                    match ret {
                        Some(r) => r.print_debug_tree(),
                        None => (),
                    }
                }
                body.print_debug_tree();
            }
            ItemKind::Struct { name, fields } => {
                debug_tree::add_branch!("SStructDeclaration: ");
                debug_tree::add_leaf!("T: {:?}", name.name.kind);
                {
                    debug_tree::add_branch!("fields: ");
                    for field in fields {
                        field.ty.print_debug_tree();
                    }
                }
            }
        }
    }
}
