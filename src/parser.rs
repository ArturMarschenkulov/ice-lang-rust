use crate::ast::{Expr, Item, Stmt};
use crate::token::*;

#[allow(unused_macros)]
macro_rules! get_function_name {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        &name[..name.len() - 3]
    }};
}

/*
stmt_declaration
    := stmt
    | stmt_declaration_var
    | stmt_declaration_struct
    | stmt_declaration_fn
    | ';'
stmt_declaration_var
    := 'var' (':' type) ('=' expr) ';'

stmt_declaration_fn
    := 'fn' identifier '(' parameters? ')' expr_block ';'
stmt
    := stmt_print
    | stmt_expression
stmt_print
    := expr ';'
stmt_expression
    := expr ';'

expr
    := expr_if
    | expr_while
    | expr_block
    | expr_assignment

expr_assignment
    := identifier '=' expr

expr_if
    := 'if' expr expr_block expr_else?
expr_else
    := 'else' (expr_if | expr_block)
expr_while
    := 'while' expr expr_block
expr_for
    := 'for' (stmt_declaration_var) ';' (expr) ';' (expr) expr_block


expr_block := '{' (stmt_declaration)* expr '}'
expr_unary := expr operator
expr_binary := expr operator expr
expr_primary := ident | 'true' | 'false' | expr_group | string | number

 */

/*
   prodtype, sumtype, subtype

*/

/*
    (0..10).collect::<Vec<_>>();
    Vec::<u8>::with_capacity(1024);

    (0..10).collect<Vec<_>>();
    Vec<u8>::with_capacity(1024);

    (0..10).collect{Vec{_}}();
    Vec{u8}::with_capacity(1024);
*/

enum Delimiter {
    Parenthesis, // ( )
    Brace,       // { }
    Bracket,     // [ ]
}

pub fn get_ast_from_tokens(tokens: Vec<Token>) -> Vec<Stmt> {
    Parser::new().parse(tokens)
}

fn get_unary_operator_precedence(token: &Token) -> i32 {
    use PunctuatorKind::*;
    use TokenKind::*;
    match token.kind {
        Punctuator(Plus) | Punctuator(Minus) | Punctuator(Exclamation) => 6,
        _ => 0,
    }
}
fn get_binary_operator_precedence(token: &Token) -> i32 {
    use PunctuatorKind::*;
    use TokenKind::*;
    match token.kind {
        Punctuator(Asterisk) | Punctuator(Slash) | Punctuator(Percent) => 5,
        Punctuator(Plus) | Punctuator(Minus) => 4,
        Punctuator(Less)
        | Punctuator(LessEqual)
        | Punctuator(Greater)
        | Punctuator(GreaterEqual)
        | Punctuator(EqualEqual)
        | Punctuator(BangEqual) => 3,
        Punctuator(Ampersand) | Punctuator(AmpersandAmpersand) => 2,
        Punctuator(VerticalBar) | Punctuator(PipePipe) => 1,
        _ => 0,
    }
}
#[derive(Clone, Copy)]
enum Associativity {
    Left,
    Right,
    None,
}
#[derive(Clone)]
struct BindingPower {
    left: f32,
    right: f32,
}
impl BindingPower {
    fn new(precedence: u32, associativity: Associativity) -> Self {
        let prec = precedence as f32;
        match associativity {
            Associativity::Left => BindingPower {
                left: prec + 0.1,
                right: prec - 0.1,
            },
            Associativity::Right => BindingPower {
                left: prec - 0.1,
                right: prec + 0.1,
            },
            Associativity::None => BindingPower {
                left: prec,
                right: prec,
            },
        }
    }
    fn is_zero(&self) -> bool {
        self.left == 0.0 && self.right == 0.0
    }
}
fn get_infix_binding_power(token: &Token) -> BindingPower {
    use std::collections::HashMap;
    use Associativity::*;
    use PunctuatorKind::*;
    use TokenKind::*;

    // Right now this is inefficeint. But the point is to slowly extract that logic to a more global scope
    // This location for this logic is temporary anyway, since custom operators will be supported,
    // meaning that the one has to collect the map for that in the typechecker anyway.

    let arr = [
        //
        (Punctuator(Asterisk), 7, Left),
        (Punctuator(Slash), 7, Left),
        (Punctuator(Percent), 7, Left),
        //
        (Punctuator(Plus), 6, Left),
        (Punctuator(Minus), 6, Left),
        //
        (Punctuator(Less), 4, None),
        (Punctuator(LessEqual), 4, None),
        (Punctuator(Greater), 3, None),
        (Punctuator(GreaterEqual), 4, None),
        (Punctuator(EqualEqual), 4, None),
        (Punctuator(BangEqual), 4, None),
        //
        (Punctuator(Ampersand), 3, None),
        (Punctuator(VerticalBar), 2, None),
        //
        (Punctuator(Equal), 1, Right),
    ];

    // let s = |tk: &TokenKind| format!("{:?}", tk);
    // Workaround, so that we don't have to implement stuff only for the HashMap.
    fn s(tk: &TokenKind) -> String {
        format!("{:?}", tk)
    }
    arr.iter()
        .map(|(tk, prec, asso)| (s(tk), BindingPower::new(*prec, *asso)))
        .collect::<HashMap<String, BindingPower>>()
        .get(&s(&token.kind))
        .unwrap_or(&BindingPower::new(0, None))
        .clone()
}
struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new() -> Self {
        Self {
            tokens: Vec::new(),
            current: 0,
        }
    }
    fn parse(&mut self, tokens: Vec<Token>) -> Vec<Stmt> {
        self.tokens = tokens;
        let mut statements: Vec<Stmt> = Vec::new();

        while !self.is_at_end() {
            statements.push(*self.parse_stmt_decl());
        }
        statements
    }
    fn parse_stmt_decl_type_struct(&mut self) -> Box<Stmt> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        self.eat(&Keyword(Type)).unwrap();
        let name = self.eat_with(&TokenKind::is_identifier).unwrap().clone();

        let _eq_sign = self.eat(&Punctuator(Equal)).unwrap();

        self.eat(&Keyword(Struct)).unwrap();
        self.eat(&Punctuator(LeftBrace)).unwrap();

        let fields = self.parse_delim_seq(
            &Punctuator(LeftBrace),
            &Punctuator(RightBrace),
            &Punctuator(Comma),
            ("field", "name", "type"),
        );

        let strct = Stmt::Definition(Item::Struct {
            name,
            fields,
        });
        Box::from(strct)
    }
    fn parse_stmt_decl_type(&mut self) -> Box<Stmt> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        // NOTE: Right now no tokens are consumed in this function
        //       This should be delegated for now to other functions.
        //       This function only finds one to which function to dispatch to

        self.check_at(&Keyword(Type), 0).unwrap();
        let name = self
            .check_at_with(1, &TokenKind::is_identifier)
            .unwrap()
            .clone();
        let _eq_sign = self.check_at(&Punctuator(Equal), 2).unwrap();

        let ty = match self.peek(3).unwrap().kind {
            Keyword(Struct) => self.parse_stmt_decl_type_struct(),
            _ => (todo!()),
        };
        ty
    }
    fn parse_stmt_decl(&mut self) -> Box<Stmt> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        let s = match &self.peek(0).unwrap().kind {
            Keyword(Var) => self.parse_stmt_decl_var(),
            Keyword(Fn) => self.parse_stmt_decl_fn(),
            Keyword(Type) => self.parse_stmt_decl_type(),
            Punctuator(Semicolon) => {
                self.advance();
                Box::from(Stmt::NoOperation)
            }
            _ => self.parse_stmt(),
        };

        match *s {
            Stmt::Definition(Item::Var { .. }) => {
                let tok = self.eat(&Punctuator(Semicolon)).unwrap();
            }
            // ref stmt@Stmt::NoOperation => println!("{:?}", stmt),
            _ => (),
        }
        s
    }

    /// There 3 variable declaration types.
    /// 1. var <ident> := <expr>;
    /// 2. var <ident> : <type> = <expr>;
    /// 3. var <ident> : <type>;
    fn parse_stmt_decl_var(&mut self) -> Box<Stmt> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        self.eat(&Keyword(Var)).unwrap();

        let name = self
            .eat_identifier()
            .expect("Expected variable name")
            .clone();

        self.eat(&Punctuator(Colon)).unwrap();
        let ty = self.eat_identifier().cloned().ok();

        let expr = match self.eat(&Punctuator(Equal)) {
            Ok(_) => Some(self.parse_expr()),
            Err(_) if ty.is_some() => None,
            Err(_) => panic!("Expected equal sign"),
        };

        let var_decl = Stmt::Definition(Item::Var {
            var: name,
            ty: ty,
            init: expr,
        });

        self.check(&Punctuator(Semicolon))
            .expect("Expected ';' after variable declaration");

        Box::from(var_decl)
    }
    fn parse_delim_seq(
        &mut self,
        start: &TokenKind,
        end: &TokenKind,
        sep: &TokenKind,
        s: (&str, &str, &str),
    ) -> Vec<(Token, Token)> {
        use PunctuatorKind::*;
        use TokenKind::*;

        let mut result = Vec::new();
        self.eat(start).unwrap();
        while self.check(end).is_err() {
            let name = self
                .eat_identifier()
                .unwrap_or_else(|_| panic!("Expected {} {}", s.0, s.1))
                .clone();
            self.eat(&Punctuator(Colon))
                .unwrap_or_else(|_| panic!("Expected ':' after {} {}", s.0, s.1));
            let ty = self
                .eat_identifier()
                .unwrap_or_else(|_| panic!("Expected {} {}", s.0, s.2))
                .clone();
            let eaten_sep = self.eat(sep);
            if eaten_sep.is_err() {
                self.check(end)
                    .unwrap_or_else(|_| panic!("Expected '{}' after {} {}", end.as_str(), s.0, s.1));
            }
            result.push((name, ty));
        }
        self.eat(end)
            .unwrap_or_else(|_| panic!("Expected '{}' after {} parameters", end.as_str(), s.0));
        result
    }
    fn parse_stmt_decl_fn(&mut self) -> Box<Stmt> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        self.eat(&Keyword(Fn)).unwrap();

        let name = self
            .eat_identifier()
            .expect("Expected function name")
            .clone();

        let parameters = self.parse_delim_seq(
            &Punctuator(LeftParen),
            &Punctuator(RightParen),
            &Punctuator(Comma),
            ("parameter", "name", "type"),
        );

        let ret_symbol = self.peek(0).unwrap().clone().kind;
        if [Punctuator(Colon)].contains(&ret_symbol) {
            let _ = self.eat(&ret_symbol);
        }
        let ret_type = self.eat_identifier().cloned().ok();

        self.check(&Punctuator(LeftBrace))
            .expect("Expected '{' after function name");
        let block = self.parse_expr_block();
        self.eat(&Punctuator(RightBrace))
            .expect("Expected '}' after function body");
        Box::from(Stmt::Definition(Item::Fn {
            name,
            params: parameters,
            ret: ret_type,
            body: block,
        }))
    }
    fn parse_stmt(&mut self) -> Box<Stmt> {
        let token = self.peek(0);
        match &token.unwrap().kind {
            _ => self.parse_stmt_expression(),
        }
    }
    fn parse_stmt_expression(&mut self) -> Box<Stmt> {
        use PunctuatorKind::*;
        use TokenKind::*;

        let expr = self.parse_expr();

        match *expr {
            Expr::Block(..) | Expr::If(..) | Expr::While(..) | Expr::For(..) => {
                // self.eat_tok(&Punctuator(Semicolon))
                //     .expect("Expected ';' after expression");
            }
            _ => {
                self.eat(&Punctuator(Semicolon))
                    .expect("Expected ';' after expression");
            }
        }

        Box::from(Stmt::Expression(expr))
    }
    fn parse_expr(&mut self) -> Box<Expr> {
        self.parse_expr_binary(0.0)
    }
    fn parse_expr_if(&mut self) -> Box<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        self.eat(&Keyword(If)).expect("Expected 'if'");
        let condition = self.parse_expr();
        self.check(&Punctuator(LeftBrace))
            .expect("Expected '{' after if condition");
        let then_branch = self.parse_expr_block();
        self.eat(&Punctuator(RightBrace))
            .expect("Expected '}' after if body");

        let else_branch = match self.peek(0).unwrap().kind {
            Keyword(Else) => Some(self.parse_expr_else()),
            _ => None,
        };
        Box::from(Expr::If(condition, then_branch, else_branch))
    }
    fn parse_expr_else(&mut self) -> Box<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;
        self.eat(&Keyword(Else)).expect("Expected 'else'");
        match self.peek(0).unwrap().kind {
            Keyword(If) => {
                //self.advance();
                self.parse_expr_if()
            }
            _ => {
                self.check(&Punctuator(LeftBrace))
                    .expect("Expected '{' after else");
                let e = self.parse_expr_block();
                self.eat(&Punctuator(RightBrace))
                    .expect("Expected '}' after else body");
                e
            }
        }
    }
    fn parse_expr_while(&mut self) -> Box<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;
        self.eat(&Keyword(While)).unwrap();
        let condition = self.parse_expr();
        self.check(&Punctuator(LeftBrace))
            .expect("Expect '{' after while condition.");
        let while_body = self.parse_expr_block();
        self.eat(&Punctuator(RightBrace))
            .expect("Expect '}' after while body.");
        Box::from(Expr::While(condition, while_body))
    }
    fn parse_expr_for(&mut self) -> Box<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        self.eat(&Keyword(For)).unwrap();
        let initilizer = match self.peek(0).unwrap().kind {
            Keyword(Var) => Some(self.parse_stmt_decl_var()),
            Punctuator(Semicolon) => {
                self.advance();
                None
            }
            _ => Some(self.parse_stmt_expression()),
        };

        let condition = self.parse_expr();
        self.eat(&Punctuator(Semicolon))
            .expect("Expected ';' after for condition");

        let itr_expr = self.parse_expr();
        self.check(&Punctuator(LeftBrace))
            .expect("Expected '{' after for iterator expression");

        let while_body = self.parse_expr_block();
        self.eat(&Punctuator(RightBrace))
            .expect("Expected '}' after for body");
        Box::from(Expr::For(
            initilizer.unwrap(),
            condition,
            itr_expr,
            while_body,
        ))
    }
    fn parse_expr_block(&mut self) -> Box<Expr> {
        use PunctuatorKind::*;
        use TokenKind::*;
        // Helper function (or rather closure).
        fn should_add(this: &Parser) -> bool {
            let mut i = 0_isize;
            while this.check_at(&Punctuator(RightBrace), i).is_err() {
                if this.check_at(&Punctuator(Semicolon), i).is_ok() {
                    return true;
                }
                i += 1;
            }
            false
        }

        self.eat(&Punctuator(LeftBrace)).unwrap();
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.is_at_end() && self.check(&Punctuator(LeftBrace)).is_err() {
            if should_add(self) {
                statements.push(*self.parse_stmt_decl());
            } else {
                break;
            }
        }

        let expr = match self.eat(&Punctuator(RightBrace)) {
            Ok(_) => None,
            Err(_) => {
                let e = Some(self.parse_expr());
                match self.eat(&Punctuator(RightBrace)) {
                    Ok(_) => e,
                    Err(_) => panic!("{}", "Expected '}' after block"),
                }
            }
        };

        Box::from(Expr::Block(statements, expr))
    }
    fn parse_expr_binary(&mut self, prev_bp: f32) -> Box<Expr> {
        let un_op_prec = get_unary_operator_precedence(self.peek(0).unwrap());
        let mut left = if un_op_prec != 0 && un_op_prec >= prev_bp as i32 {
            let op = self.peek(0).unwrap().clone();
            self.advance();
            let right = self.parse_expr_binary(un_op_prec as f32);
            Box::from(Expr::Unary(op, right))
        } else {
            self.parse_expr_primary()
        };

        loop {
            let binding_power = get_infix_binding_power(self.peek(0).unwrap());
            if binding_power.is_zero() || binding_power.left < prev_bp {
                break;
            } else if binding_power.left == prev_bp {
                // TODO: This is an error. The operators have the same precedence and are non-associative.
                //       We should probably handle this.
                panic!(
                    "Operators have the same precedence and are non-associative. This is not {:?}",
                    self.peek(0)
                );
            }

            let operator = self.peek(0).unwrap().clone();
            self.advance();
            let right = self.parse_expr_binary(binding_power.right);
            left = Box::from(Expr::Binary(left, operator.clone(), right));
        }
        left
    }
    fn parse_expr_primary(&mut self) -> Box<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;
        let token = self.peek(0).unwrap().clone();

        let expr = match token.kind {
            Literal(lit) => {
                self.advance();
                Expr::Literal(lit)
            }
            Identifier(_) => {
                if self.check_at(&Punctuator(LeftParen), 1).is_ok() {
                    *self.parse_expr_fn_call()
                } else {
                    let mut ids = Vec::new();
                    let i = self.eat_identifier().unwrap().clone();
                    ids.push(i);

                    while self.eat(&Punctuator(Colon)).is_ok() {
                        self.eat(&Punctuator(Colon)).unwrap();
                        let i = self.eat_identifier().unwrap().clone();
                        ids.push(i);
                    }

                    let actual_id = ids.pop().unwrap();
                    let actual_path = if ids.is_empty() { None } else { Some(ids) };

                    Expr::Symbol {
                        name: actual_id,
                        path: actual_path,
                    }
                }
            }

            Keyword(If) => *self.parse_expr_if(),
            Keyword(While) => *self.parse_expr_while(),
            Keyword(For) => *self.parse_expr_for(),
            Punctuator(LeftParen) => {
                self.advance(); //skip the (
                let left = self.parse_expr();
                self.eat(&Punctuator(RightParen))
                    .expect("Expected ')' after expression");
                Expr::Grouping(left)
            }
            Punctuator(LeftBrace) => {
                let stmts = self.parse_expr_block();
                self.eat(&Punctuator(RightBrace))
                    .expect("Expected '}' after expression");
                *stmts
            }
            unhandled => {
                panic!("{:?}", &unhandled)
            }
        };
        Box::from(expr)
    }
    fn parse_expr_fn_call(&mut self) -> Box<Expr> {
        use PunctuatorKind::*;
        use TokenKind::*;

        let callee_name = self.eat_identifier().cloned().unwrap();

        let callee = Box::from(Expr::Symbol {
            name: callee_name,
            path: None,
        });
        self.eat(&Punctuator(LeftParen))
            .expect("Expected '(' after function name");

        let mut arguments: Vec<Expr> = Vec::new();
        while self.peek(0).unwrap().kind != Punctuator(RightParen) {
            arguments.push(*self.parse_expr());
            if self.peek(0).unwrap().kind != Punctuator(RightParen) {
                self.eat(&Punctuator(Comma))
                    .expect("Expected ',' after argument");
            }
        }

        self.eat(&Punctuator(RightParen))
            .expect("Expected ')' after arguments");

        Box::from(Expr::FnCall(callee, arguments))
    }
}
impl Parser {
    // fn peek(&self, offset: i32) -> &Token {
    //     let index = self.current as i32 + offset;
    //     if index as usize >= self.tokens.len() {
    //         panic!("Index out of bounds");
    //         //&self.tokens[self.current]
    //     } else {
    //         &self.tokens[index as usize]
    //     }
    // }
    fn peek(&self, offset: isize) -> Option<&Token> {
        let new_offset = if offset.is_negative() {
            self.current.checked_sub(-offset as usize)
        } else if offset.is_positive() {
            self.current.checked_add(offset as usize)
        } else {
            Some(self.current)
        };

        match new_offset {
            Some(new_offset) => self.tokens.get(new_offset),
            None => None,
        }
    }

    fn check_at(&self, kind: &TokenKind, offset: isize) -> Result<&Token, Option<&Token>> {
        self.check_at_with(offset, |tk| tk == kind)
    }
    fn check(&self, kind: &TokenKind) -> Result<&Token, Option<&Token>> {
        self.check_at(kind, 0)
    }
    fn check_at_with<F>(&self, offset: isize, func: F) -> Result<&Token, Option<&Token>>
    where
        Self: Sized,
        F: Fn(&TokenKind) -> bool,
    {
        let peeked = self.peek(offset);
        match peeked {
            Some(token) if func(&token.kind) => Ok(token),
            Some(_) => Err(peeked),
            None => Err(None),
        }
    }
    fn eat(&mut self, kind: &TokenKind) -> Result<&Token, Option<&Token>> {
        self.eat_with(|t| t == kind)
    }
    fn eat_identifier(&mut self) -> Result<&Token, Option<&Token>> {
        self.eat_with(&TokenKind::is_identifier)
    }
    fn eat_punctuator(&mut self) -> Result<&Token, Option<&Token>> {
        self.eat_with(&TokenKind::is_punctuator)
    }
    fn eat_keyword(&mut self) -> Result<&Token, Option<&Token>> {
        self.eat_with(&TokenKind::is_keyword)
    }
    fn eat_literal(&mut self) -> Result<&Token, Option<&Token>> {
        self.eat_with(&TokenKind::is_literal)
    }
    fn eat_with<F>(&mut self, func: F) -> Result<&Token, Option<&Token>>
    where
        Self: Sized,
        F: Fn(&TokenKind) -> bool,
    {
        // This is an ugly workaround for a limitation of the borrow checker.
        // Rust's new polonius would be able to handle this better,
        // but for now we'll just use this.
        // TODO: If polonius is ready, use the better version.

        if let Ok(..) = self.check_at_with(0, &func) {
            self.advance()
        }

        match self.peek(-1) {
            Some(token) if func(&token.kind) => Ok(token),
            peeked @ Some(..) => Err(peeked),
            None => Err(None),
        }

        // // After polonius is ready, this SHOULD work
        // match self.check_at_with(0, &func) {
        //     Ok(token) => {
        //         self.advance();
        //         Ok(token)
        //     },
        //     Err(err) => Err(err),
        // }
    }
    fn advance(&mut self) {
        if !self.is_at_end() {
            self.current += 1;
        }
    }
    fn is_at_end(&self) -> bool {
        use SpecialKeywordKind::*;
        use TokenKind::*;
        self.peek(0).unwrap().kind == SpecialKeyword(Eof)
    }
}
