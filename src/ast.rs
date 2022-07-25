use crate::token::{LiteralKind, Token, TokenKind};
use debug_tree::*;

// struct Program {
//     pub statements: Vec<Statement>,
// }
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    // pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
pub enum TyKind {
    Simple(Token),
}
#[derive(Clone, Debug, PartialEq)]
pub struct Ty {
    pub kind: TyKind,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub name: Token,
    pub ty: TyKind,
}
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
pub struct Item {
    pub kind: ItemKind,
}

// The logic of the language and the way how it is parsed do not match.
// `ExpressionWithoutSemicolon` is in this a statement, however
// in the language itself it's an expression.
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    // pub span: Span,
}

pub fn print_ast(ast: &Vec<Item>) {
    //debug_tree::add_branch!("Ast Tree");
    for ast_s in ast {
        //println!("{:#?}", ast_s);
        debug_tree::defer_print!();
        ast_s.kind.print();
    }
}

impl ExprKind {
    fn print(&self) {
        match self {
            ExprKind::Literal(lit) => {
                add_branch!("Lit {:?}", lit);
            }
            ExprKind::Grouping(expr) => {
                add_branch!("Group: ");
                expr.kind.print();
            }
            ExprKind::Binary(expr_l, op, expr_r) => {
                add_branch!("Binary: {:?}", op.kind);
                expr_l.kind.print();
                expr_r.kind.print();
            }
            ExprKind::Unary(op, expr) => {
                add_branch!("Unary: {:?}", op.kind);
                expr.kind.print();
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
                    ast_s.kind.print();
                }
                // if let Some(expr) = last_expr {
                //     expr.print();
                // }
            }
            ExprKind::If(comp, then_branch, else_branch) => {
                add_branch!("If: ");
                comp.kind.print();
                then_branch.kind.print();
                if let Some(block) = else_branch {
                    block.kind.print();
                }
            }
            ExprKind::While(comp, body) => {
                add_branch!("While: ");
                comp.kind.print();
                body.kind.print();
            }
            ExprKind::For(init, comp, expr, body) => {
                add_branch!("For: ");
                init.kind.print();
                comp.kind.print();
                expr.kind.print();
                body.kind.print();
            }
            ExprKind::FnCall(callable, parameters) => {
                add_branch!("FnCall: ");
                callable.kind.print();
                {
                    add_branch!("parameters: ");
                    for _be1 in parameters {
                        _be1.kind.print();
                    }
                }
            }
        }
    }
}
impl StmtKind {
    pub fn print(&self) {
        match self {
            StmtKind::Var { var, ty, init } => {
                add_branch!("SVarDeclaration: ");
                add_leaf!("T: {:?}", var.kind);
                add_leaf!("T: {:?}", ty);
                match init.as_ref() {
                    Some(i) => i.kind.print(),
                    None => add_leaf!("T: {:?}", init),
                }
                // init.as_ref().unwrap().print();
            }
            StmtKind::Expression(expr) => {
                add_branch!("SExpression: ");
                expr.kind.print();
            }
            StmtKind::ExpressionWithoutSemicolon(expr) => {
                add_branch!("SExpressionWithoutSemicolon: ");
                expr.kind.print();
            }
            StmtKind::Item(item) => {
                item.kind.print();
            }
            StmtKind::NoOperation => {
                add_branch!("SNoOp");
            }
        }
    }
}

impl ItemKind {
    pub fn print(&self) {
        match self {
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
                    let foo = |t: Ty| {
                        let st = if let TyKind::Simple(st) = t.kind {
                            Some(st)
                        } else {
                            None
                        }
                        .unwrap();
                        st
                    };
                    let s = ret.clone().map(|t| foo(t).kind.as_str());
                    add_branch!("ret: ");
                    add_leaf!("T: {:?}", s);
                }
                body.kind.print();
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
