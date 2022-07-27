use crate::token::{LiteralKind, Token, TokenKind};
use debug_tree::*;

// struct Program {
//     pub statements: Vec<Statement>,
// }
#[derive(Clone, Debug)]
pub enum ExprKind {
    Literal(LiteralKind),
    Grouping(Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>),
    Symbol {
        name: Token,
        /// `None` means there is no path to this symbol (`x`)
        /// `Some` means there is a path to this symbol (`x::y::z`),
        /// however it can be also empty (`::x`)
        path: Option<Vec<Token>>,
    }, // symbol, path
    // Block(Vec<StmtKind>, Option<Box<ExprKind>>),
    Block(Vec<Stmt>),

    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    While(Box<Expr>, Box<Expr>),
    For(Box<Stmt>, Box<Expr>, Box<Expr>, Box<Expr>),

    FnCall(Box<Expr>, Vec<Expr>),
}
#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    // pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Parameter {
    pub name: Token,
    pub ty: TyKind,
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
    Simple(Token),
}
#[derive(Clone, Debug)]
pub struct Ty {
    pub kind: TyKind,
}
#[derive(Clone, Debug)]
pub struct Field {
    pub name: Token,
    pub ty: TyKind,
}
#[derive(Clone, Debug)]
pub enum ItemKind {
    /// fn foo(x: i32) {...}
    Fn {
        name: Token,
        params: Vec<Parameter>,
        ret: Option<Ty>,
        body: Box<Expr>,
    },
    /// type Foo = struct { x: i32 }
    Struct { name: Token, fields: Vec<Field> },
    // /// type Foo = enum { A, B }
    // Enum {
    //     name: Token,
    //     variants: Vec<Field>,
    // },

    // /// type Unit = {}
    // Unit {
    //     name: Token,
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
    /// let a: i32 = 4,
    Var {
        var: Token,
        ty: Option<Ty>,
        init: Option<Box<Expr>>,
    },
    /// Expression with a trailing semicolon
    Expression(Box<Expr>),
    /// Expression without a trailing semicolon
    ExpressionWithoutSemicolon(Box<Expr>),
    // TODO: Rename it into `Item`
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

pub fn print_ast(ast: &Vec<Item>) {
    //debug_tree::add_branch!("Ast Tree");
    for ast_s in ast {
        //println!("{:#?}", ast_s);
        debug_tree::defer_print!();
        ast_s.print_debug_tree();
    }
}

trait DebugTreePrinter {
    fn print_debug_tree(&self);
}

impl DebugTreePrinter for Expr {
    fn print_debug_tree(&self) {
        match &self.kind {
            ExprKind::Literal(lit) => {
                add_branch!("Lit {:?}", lit);
            }
            ExprKind::Grouping(expr) => {
                add_branch!("Group: ");
                expr.print_debug_tree();
            }
            ExprKind::Binary(expr_l, op, expr_r) => {
                add_branch!("Binary: {:?}", op.kind);
                expr_l.print_debug_tree();
                expr_r.print_debug_tree();
            }
            ExprKind::Unary(op, expr) => {
                add_branch!("Unary: {:?}", op.kind);
                expr.print_debug_tree();
            }
            ExprKind::Symbol { name, path } => {
                let s = if path.is_some() {
                    let s = path
                        .clone()
                        .unwrap()
                        .iter()
                        .map(|t| {
                            if let TokenKind::Identifier(id) = &t.kind {
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
                add_branch!("Symbol: {}, {:?}", name.kind.as_str(), s);
            }
            // Expr::Assign(_t0, _t1, _be) => {
            //     add_branch!("Assign: {:?}", _t0.kind);
            //     _be.print();
            // }
            ExprKind::Block(stmts) => {
                add_branch!("Block: ");
                //print_ast(_vbs);
                for ast_s in stmts {
                    //println!("{:#?}", ast_s);
                    //debug_tree::defer_print!();
                    ast_s.print_debug_tree();
                }
                // if let Some(expr) = last_expr {
                //     expr.print();
                // }
            }
            ExprKind::If(comp, then_branch, else_branch) => {
                add_branch!("If: ");
                comp.print_debug_tree();
                then_branch.print_debug_tree();
                if let Some(block) = else_branch {
                    block.print_debug_tree();
                }
            }
            ExprKind::While(comp, body) => {
                add_branch!("While: ");
                comp.print_debug_tree();
                body.print_debug_tree();
            }
            ExprKind::For(init, comp, expr, body) => {
                add_branch!("For: ");
                init.print_debug_tree();
                comp.print_debug_tree();
                expr.print_debug_tree();
                body.print_debug_tree();
            }
            ExprKind::FnCall(callable, parameters) => {
                add_branch!("FnCall: ");
                callable.print_debug_tree();
                {
                    add_branch!("parameters: ");
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
                add_branch!("SVarDeclaration: ");
                add_leaf!("T: {:?}", var.kind);
                add_leaf!("T: {:?}", ty);
                match init.as_ref() {
                    Some(i) => i.print_debug_tree(),
                    None => add_leaf!("T: {:?}", init),
                }
                // init.as_ref().unwrap().print();
            }
            StmtKind::Expression(expr) => {
                add_branch!("SExpression: ");
                expr.print_debug_tree();
            }
            StmtKind::ExpressionWithoutSemicolon(expr) => {
                add_branch!("SExpressionWithoutSemicolon: ");
                expr.print_debug_tree();
            }
            StmtKind::Item(item) => {
                item.print_debug_tree();
            }
            StmtKind::NoOperation => {
                add_branch!("SNoOp");
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
                add_branch!("SFnDeclaration: ");
                add_leaf!("T: {:?}", name.kind);
                {
                    add_branch!("parameters: ");

                    for param in params {
                        let st = if let TyKind::Simple(st) = &param.ty {
                            Some(st)
                        } else {
                            None
                        }
                        .unwrap();
                        add_leaf!("T: {}: {}", param.name.kind.as_str(), st.kind.as_str());
                        //_t.print();
                    }
                }
                {
                    let f = |t: Ty| {
                        if let TyKind::Simple(st) = t.kind {
                            Some(st)
                        } else {
                            None
                        }
                        .unwrap()
                    };
                    let s = ret.clone().map(|t| f(t).kind.as_str());
                    add_branch!("ret: ");
                    add_leaf!("T: {:?}", s);
                }
                body.print_debug_tree();
            }
            ItemKind::Struct { name, fields } => {
                add_branch!("SStructDeclaration: ");
                add_leaf!("T: {:?}", name.kind);
                {
                    add_branch!("fields: ");
                    for field in fields {
                        let st = if let TyKind::Simple(st) = &field.ty {
                            Some(st)
                        } else {
                            None
                        }
                        .unwrap();
                        add_leaf!("T: {}: {}", field.name.kind.as_str(), st.kind.as_str());
                        //_t.print();
                    }
                }
            }
        }
    }
}
