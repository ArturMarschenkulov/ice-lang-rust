use crate::token::{LiteralKind, Token};
use debug_tree::*;
#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Expr {
    Literal(LiteralKind),
    Grouping(Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>),
    Symbol(Token),
    Assign(Token, Token, Box<Expr>),
    Block(Vec<Stmt>, Option<Box<Expr>>),

    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    While(Box<Expr>, Box<Expr>),
    For(Box<Stmt>, Box<Expr>, Box<Expr>, Box<Expr>),

    FnCall(Box<Expr>, Vec<Expr>),
}
#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Stmt {
    Expression(Box<Expr>),
    VarDeclaration(Token, Option<Box<Expr>>),
    FnDeclaration(Token, Vec<Token>, Box<Expr>),
    NoOperation,

    Print(Box<Expr>),
    Println(Box<Expr>),
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
            Expr::Symbol(_t) => {
                add_branch!("Symbol: {:?}", _t.kind);
            }
            Expr::Assign(_t0, _t1, _be) => {
                add_branch!("Assign: {:?}", _t0.kind);
                _be.print();
            }
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
            Stmt::VarDeclaration(_t, _obe) => {
                add_branch!("SVarDeclaration: ");
                add_leaf!("T: {:?}", _t.kind);
                _obe.as_ref().unwrap().print();
            }
            Stmt::FnDeclaration(_t, _vt, _be) => {
                add_branch!("SFnDeclaration: ");
                add_leaf!("T: {:?}", _t.kind);
                {
                    add_branch!("parameters: ");
                    for _t in _vt {
                        add_leaf!("T: {:?}", _t.kind);
                        //_t.print();
                    }
                }
                _be.print();
            }
            Stmt::NoOperation => {
                add_branch!("SNoOp");
            }
            Stmt::Print(be) => {
                add_branch!("Sprint: ");
                be.print();
            }
            Stmt::Println(be) => {
                add_branch!("Sprintln: ");
                be.print();
            }
        }
    }
}
