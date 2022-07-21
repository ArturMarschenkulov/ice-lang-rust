use crate::token::{LiteralKind, Token, TokenKind};
use debug_tree::*;

// struct Program {
//     pub statements: Vec<Statement>,
// }
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
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
    // Assign(Token, Token, Box<Expr>),
    Block(Vec<Stmt>, Option<Box<Expr>>),

    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    While(Box<Expr>, Box<Expr>),
    For(Box<Stmt>, Box<Expr>, Box<Expr>, Box<Expr>),

    FnCall(Box<Expr>, Vec<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
struct Parameter {
    name: Token,
    ty: Token,
}

// NOTE: This is a placeholder name, since right now, we can only parse one file, so a whole project will always be a file.
// Later on, one should change the name, maybe 'Crate', 'Project', 'Package', 'Program', 'Module' etc.
struct FileModule {
    items: Vec<Item>,
}
#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    /// fn foo(x: i32) {...}
    Fn {
        name: Token,
        params: Vec<(Token, Token)>,
        body: Box<Expr>,
    },
    // /// type Foo = struct { x: i32 }
    // Struct {
    //     name: Token,
    //     fields: Vec<(Token, Token)>,
    // },
}
#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Expression(Box<Expr>),
    VarDeclaration {
        var: Token,
        ty: Option<Token>,
        init: Option<Box<Expr>>,
    },
    Definition(Item),
    NoOperation,
}

pub fn print_ast(ast: &Vec<Stmt>) {
    //debug_tree::add_branch!("Ast Tree");
    for ast_s in ast {
        //println!("{:#?}", ast_s);
        debug_tree::defer_print!();
        ast_s.print();
    }
}

impl Expr {
    fn print(&self) {
        match self {
            Expr::Literal(_lk) => {
                add_branch!("Lit {:?}", _lk);
            }
            Expr::Grouping(_be) => {
                add_branch!("Group: ");
                _be.print();
            }
            Expr::Binary(_bel, _t, _ber) => {
                add_branch!("Binary: {:?}", _t.kind);
                _bel.print();
                _ber.print();
            }
            Expr::Unary(_t, _ber) => {
                add_branch!("Unary: {:?}", _t.kind);
                _ber.print();
            }
            Expr::Symbol { name, path } => {

                let s = if path.is_some() {
                    let s = path.clone().unwrap().iter().map(|t| {
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
            Expr::Block(_vbs, _obe) => {
                add_branch!("Block: ");
                //print_ast(_vbs);
                for ast_s in _vbs {
                    //println!("{:#?}", ast_s);
                    //debug_tree::defer_print!();
                    ast_s.print();
                }
                if let Some(_be) = _obe {
                    _be.print();
                }
            }
            Expr::If(_be0, _be1, _obe) => {
                add_branch!("If: ");
                _be0.print();
                _be1.print();
                if let Some(_be3) = _obe {
                    _be3.print();
                }
            }
            Expr::While(_be0, _be1) => {
                add_branch!("While: ");
                _be0.print();
                _be1.print();
            }
            Expr::For(_bs, _be0, _be1, _be2) => {
                add_branch!("For: ");
                _bs.print();
                _be0.print();
                _be1.print();
                _be2.print();
            }
            Expr::FnCall(_be, _vbe) => {
                add_branch!("FnCall: ");
                _be.print();
                {
                    add_branch!("parameters: ");
                    for _be1 in _vbe {
                        _be1.print();
                    }
                }
            }
        }
    }
}
impl Stmt {
    pub fn print(&self) {
        match self {
            Stmt::Expression(_be) => {
                add_branch!("SExpression: ");
                _be.print();
            }
            Stmt::VarDeclaration { var, ty, init } => {
                add_branch!("SVarDeclaration: ");
                add_leaf!("T: {:?}", var.kind);
                add_leaf!("T: {:?}", ty);
                match init.as_ref() {
                    Some(i) => i.print(),
                    None => add_leaf!("T: {:?}", init),
                }
                // init.as_ref().unwrap().print();
            }
            Stmt::Definition(Item::Fn { name, params, body }) => {
                add_branch!("SFnDeclaration: ");
                add_leaf!("T: {:?}", name.kind);
                {
                    add_branch!("parameters: ");
                    for _t in params {
                        add_leaf!("T: {}: {}", _t.0.kind.as_str(), _t.1.kind.as_str());
                        //_t.print();
                    }
                }
                body.print();
            }
            Stmt::NoOperation => {
                add_branch!("SNoOp");
            }
        }
    }
}
