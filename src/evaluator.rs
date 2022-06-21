//use crate::parser::*;
use crate::ast::{Expr, Stmt};
use crate::token::*;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IceFunction {
    pub params: Vec<Token>,
    pub body: Box<Expr>,
}

impl IceFunction {
    /*fn get_arity() -> usize {
        unimplemented!()
    }*/

    //fn call(&mut self, evaluator: &mut Evaluator, arguments: &Vec<Box<Expr>>) -> IceObject {
    //    for i in 0..arguments.len() {
    //        let e = self.evaluate_expr(&arguments[i]);
    //        evaluator.environment.define(self.params[i].clone(), e);
    //    }
    //    let e = self.evaluate_expr(&self.body);
    //}
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IceObject {
    Literal(LiteralKind),
    Function(IceFunction),
}

#[derive(Debug, Clone)]
struct SymbolStack {
    scope_stack: Vec<HashMap<Token, Option<IceObject>>>,
    current_scope_level: usize,
}
impl SymbolStack {
    fn new() -> Self {
        let scope: HashMap<Token, Option<IceObject>> = HashMap::new();
        let mut s = Vec::new();
        s.push(scope);
        Self {
            scope_stack: s,
            current_scope_level: 0,
        }
    }
    fn define(&mut self, token: Token, value: Option<IceObject>) {
        self.scope_stack[self.current_scope_level].insert(token, value);
    }
    fn get(&self, token: &Token) -> Option<IceObject> {
        let mut sc = self.current_scope_level.clone();
        let res;
        loop {
            if let Some(a) = self.scope_stack[sc].get(token) {
                res = a;
                return res.clone();
            }
            if sc == 0 {
                panic!("Identifier {:?} not found", &token);
            }
            sc -= 1;
        }
    }
    fn assign(&mut self, token: &Token, value: IceObject) {
        if self.scope_stack[self.current_scope_level].contains_key(&token.clone()) {
            self.scope_stack[self.current_scope_level].insert(token.clone(), Some(value));
        } else {
            let mut sc = self.current_scope_level.clone();
            while sc > 0 {
                sc -= 1;
                if self.scope_stack[sc].contains_key(&token.clone()) {
                    self.scope_stack[sc].insert(token.clone(), Some(value));
                    break;
                }
            }
        }
    }
    fn push(&mut self) {
        let a: HashMap<Token, Option<IceObject>> = HashMap::new();
        self.current_scope_level += 1;
        self.scope_stack.push(a);
    }
    fn pop(&mut self) {
        self.scope_stack.pop();
        self.current_scope_level -= 1;
    }
}

pub fn get_evaluation_from_ast(statements: &Vec<Box<Stmt>>) {
    Evaluator::new().evaluate_stmts(statements);
}

pub struct Evaluator {
    environment: SymbolStack,
}
impl Evaluator {
    fn new() -> Self {
        Self {
            environment: SymbolStack::new(),
        }
    }
    fn evaluate_stmts(&mut self, statements: &Vec<Box<Stmt>>) {
        for stmt in statements {
            self.evaluate_stmt(stmt);
        }
    }
    fn evaluate_stmt(&mut self, statement: &Box<Stmt>) {
        match **statement {
            Stmt::VarDeclaration(ref name, ref init) => match init {
                Some(expr) => {
                    let val = self.evaluate_expr(expr);
                    self.environment.define(name.clone(), val.clone());
                }
                None => {
                    self.environment.define(name.clone(), None);
                }
            },
            Stmt::FnDeclaration(ref name, ref parameters, ref body) => {
                //println!("FnDeclaration");
                let func = IceObject::Function(IceFunction {
                    params: parameters.clone(),
                    body: body.clone(),
                });
                self.environment.define(name.clone(), Some(func));
            }
            Stmt::Expression(ref expr) => {
                self.evaluate_expr(expr);
            }
            Stmt::Print(ref expr) => {
                let res = match self.evaluate_expr(expr) {
                    Some(a) => a,
                    None => {
                        println!("{:?}", &self.environment);
                        panic!("Uninitialized variable: {:?}", &expr)
                    }
                };
                match res {
                    IceObject::Literal(lit) => {
                        match lit {
                            LiteralKind::Boolean(bool) => print!("{}", bool),
                            LiteralKind::Integer(int) => print!("{}", int),
                            //LiteralKind::Float(float) => println!("{}", float),
                            LiteralKind::String(string) => print! {"{}", string},
                            LiteralKind::Unit => print! {"()"},
                        }
                    }
                    _ => unimplemented!(),
                };
            }
            Stmt::Println(ref expr) => {
                let res = match self.evaluate_expr(expr) {
                    Some(a) => a,
                    None => {
                        println!("{:?}", &self.environment);
                        panic!("Uninitialized variable: {:?}", &expr)
                    }
                };
                match res {
                    IceObject::Literal(lit) => {
                        match lit {
                            LiteralKind::Boolean(bool) => println!("{}", bool),
                            LiteralKind::Integer(int) => println!("{}", int),
                            //IceObject::Literal(LiteralKind::Float(float)) => println!("{}", float),
                            LiteralKind::String(string) => println! {"{}", string},
                            LiteralKind::Unit => println! {"()"},
                        }
                    }
                    _ => unimplemented!(),
                };
            }
            Stmt::NoOperation => {}
        }
    }
    fn evaluate_expr(&mut self, expression: &Box<Expr>) -> Option<IceObject> {
        match **expression {
            Expr::Binary(ref l, ref t, ref r) => {
                let left = self.evaluate_expr(l).unwrap();
                let right = self.evaluate_expr(r).unwrap();

                use LiteralKind::*;
                use PunctuatorKind::*;
                use TokenKind::*;
                let res = match (left, right) {
                    (IceObject::Literal(lit0), IceObject::Literal(lit1)) => match (lit0, lit1) {
                        (Integer(ls), Integer(rs)) => match t.kind {
                            Punctuator(Plus) => LiteralKind::Integer(ls + rs),
                            Punctuator(Minus) => LiteralKind::Integer(ls - rs),
                            Punctuator(Star) => LiteralKind::Integer(ls * rs),
                            Punctuator(Slash) => LiteralKind::Integer(ls / rs),
                            Punctuator(Percent) => LiteralKind::Integer(ls % rs),

                            Punctuator(Less) => LiteralKind::Boolean(ls < rs),
                            Punctuator(LessEqual) => LiteralKind::Boolean(ls <= rs),
                            Punctuator(Greater) => LiteralKind::Boolean(ls > rs),
                            Punctuator(GreaterEqual) => LiteralKind::Boolean(ls >= rs),
                            Punctuator(EqualEqual) => LiteralKind::Boolean(ls == rs),
                            Punctuator(BangEqual) => LiteralKind::Boolean(ls != rs),

                            _ => panic!("Both operands have to be integers"),
                        },
                        (Boolean(ls), Boolean(rs)) => match t.kind {
                            Punctuator(AmpersandAmpersand) => LiteralKind::Boolean(ls && rs),
                            Punctuator(PipePipe) => LiteralKind::Boolean(ls || rs),
                            _ => panic!("Both operands have to be booleans"),
                        },
                        (String(ls), String(rs)) => match t.kind {
                            Punctuator(Plus) => LiteralKind::String(ls + &rs),
                            _ => panic!("Only + is supported for {} and {}", ls, rs),
                        },
                        _ => unimplemented!(),
                    },

                    (ls, rs) => panic!("Binary expression not supported for {:?} and {:?}", ls, rs),
                };
                Some(IceObject::Literal(res))
            }
            Expr::Unary(ref t, ref r) => {
                let right = self.evaluate_expr(r).unwrap();
                use LiteralKind::*;
                use PunctuatorKind::*;
                use TokenKind::*;
                let res = match right {
                    IceObject::Literal(lit) => match lit {
                        Integer(ref ls) => match t.kind {
                            // TODO: Fix this operation. Either remove unary minus entirely or make it overflow.
                            // Right now I created a bug, because it simply does a noop.
                            Punctuator(Minus) => LiteralKind::Integer(*ls),
                            _ => panic!("Integers support only Minus as a unary operation"),
                        },
                        Boolean(ref ls) => match t.kind {
                            Punctuator(Bang) => LiteralKind::Boolean(!*ls),
                            _ => panic!("Booleans support only Bang as a unary operation"),
                        },
                        String(_) => match t.kind {
                            _ => panic!("String don't support any unary operations yet"),
                        },
                        Unit => match t.kind {
                            _ => panic!("The unit type doesn't support operations"),
                        },
                    },
                    IceObject::Function(_) => unimplemented!(),
                };
                Some(IceObject::Literal(res))
            }
            Expr::Grouping(ref expr) => self.evaluate_expr(expr),
            Expr::Literal(ref expr) => Some(IceObject::Literal(expr.clone())),
            Expr::Symbol(ref token) => {
                if let Some(k) = self.environment.get(token) {
                    Some(k)
                } else {
                    None
                }
            }
            Expr::Assign(ref l, _, ref r) => match (l.kind.clone(), *r.clone()) {
                (TokenKind::Identifier(_), Expr::Assign(_, _, _)) => {
                    let right = self.evaluate_expr(r);
                    self.environment.assign(l, right.clone().unwrap());
                    right
                }
                (TokenKind::Identifier(_), n) => {
                    let right = self.evaluate_expr(&Box::from(n));
                    self.environment.assign(l, right.clone().unwrap());
                    right
                }
                (l, r) => {
                    panic!("Assignment not defined for {:?} and {:?}", &l, &r)
                }
            },
            Expr::Block(ref stmts, ref expr) => match stmts {
                statements => {
                    self.environment.push();
                    for statement in statements {
                        self.evaluate_stmt(statement);
                    }
                    let expression = match expr {
                        Some(e) => self.evaluate_expr(e),
                        None => Some(IceObject::Literal(LiteralKind::Unit)),
                    };
                    self.environment.pop();
                    expression
                }
            },
            Expr::If(ref condition_expression, ref then_block, ref else_block) => {
                if let Some(a) = self.evaluate_expr(condition_expression) {
                    match a {
                        IceObject::Literal(lit) => match lit {
                            LiteralKind::Boolean(true) => self.evaluate_expr(then_block),
                            LiteralKind::Boolean(false) => {
                                if let Some(a) = else_block {
                                    self.evaluate_expr(a)
                                } else {
                                    None
                                }
                            }
                            _ => panic!(),
                        },
                        _ => {
                            panic!("the condition of an if expression has to evaluate to a boolean")
                        }
                    }
                } else {
                    None
                }
            }
            Expr::While(ref condition_expression, ref while_body) => {
                self.environment.push();
                while let Some(cond_res) = self.evaluate_expr(&condition_expression) {
                    match cond_res {
                        IceObject::Literal(LiteralKind::Boolean(true)) => {
                            match *while_body.clone() {
                                Expr::Block(statements, expression) => {
                                    for statement in statements {
                                        self.evaluate_stmt(&statement);
                                    }
                                    if let Some(express) = expression {
                                        self.evaluate_expr(&express);
                                    }
                                }
                                _ => panic!("Implement error message!"),
                            }
                        }
                        IceObject::Literal(LiteralKind::Boolean(false)) => break,
                        _ => {
                            panic!(
                                "the condition of an while expression has to evaluate to a boolean"
                            )
                        }
                    };
                }
                self.environment.pop();
                Some(IceObject::Literal(LiteralKind::Unit))
            }
            Expr::For(
                ref init_statement,
                ref condition_expression,
                ref iteration_expression,
                ref for_body,
            ) => {
                self.environment.push();
                self.evaluate_stmt(init_statement);
                while let Some(cond_res) = self.evaluate_expr(&condition_expression) {
                    match cond_res {
                        IceObject::Literal(LiteralKind::Boolean(true)) => match *for_body.clone() {
                            Expr::Block(statements, expression) => {
                                for statement in statements {
                                    self.evaluate_stmt(&statement);
                                }
                                if let Some(express) = expression {
                                    self.evaluate_expr(&express);
                                }
                            }
                            _ => panic!("Implement error message!"),
                        },
                        IceObject::Literal(LiteralKind::Boolean(false)) => break,
                        _ => {
                            panic!(
                                "the condition of an for expression has to evaluate to a boolean"
                            )
                        }
                    }
                    self.evaluate_expr(iteration_expression);
                }
                self.environment.pop();
                Some(IceObject::Literal(LiteralKind::Unit))
            }

            Expr::FnCall(ref callee, ref args) => {
                if let Expr::Symbol(sym) = *callee.clone() {
                    if let Some(IceObject::Function(func)) = self.environment.get(&sym) {
                        // Check arity}
                        if func.params.len() != args.len() {
                            panic!(
                                "Expected {} arguments, found {} arguments",
                                func.params.len(),
                                args.len()
                            )
                        }

                        self.environment.push();
                        //func.call(self, args);
                        for (i, _item) in args.iter().enumerate() {
                            let e = self.evaluate_expr(&args[i]);
                            self.environment.define(func.params[i].clone(), e);
                        }
                        let e = self.evaluate_expr(&func.body);

                        self.environment.pop();
                        e
                    } else {
                        panic!()
                    }
                } else {
                    panic!()
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::Ice;

    #[test]
    #[should_panic]
    fn test_scope_0() {
        let text = r#"
            var a;
            {
                a = 2;
            }
            print a;
        "#;
        Ice {}.run(&mut String::from(text));
    }
    #[test]
    #[should_panic]
    fn test_scope_1() {
        let text = r#"

            {
                var a;
            }
            a = 1;
            print a;
        "#;
        Ice {}.run(&mut String::from(text));
    }

    #[test]
    fn test_nested_var_declarations() {
        let text = r#"
            var a = 2;
            var b = {
                var a = {
                    var ll = 22;
                    var ii = 44;
                    ll + ii
                };
                var h = 7;
                a + h
            };
        "#;
        Ice {}.run(&mut String::from(text));
    }

    #[test]
    fn test_simple_var_declaration() {
        let text = r#"
            var a = 2;
        "#;
        Ice {}.run(&mut String::from(text));
    }

    #[test]
    fn test_if_expression() {
        let text = r#"
            var a = 5;
            if true {
                a = 10;
            } else {
                a = 40;
            };
        "#;
        Ice {}.run(&mut String::from(text));
    }

    #[test]
    fn test_while_expression() {
        let text = r#"
            var a = 4;
            while a < 10 {
                a = a + 1;
            };
        "#;
        Ice {}.run(&mut String::from(text));
    }
    #[test]
    fn test_for_expression() {
        let text = r#"
            for var a = 4; a < 10; a = a + 1 {
            };
        "#;
        Ice {}.run(&mut String::from(text));
    }
}
