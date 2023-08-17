use crate::ast::{Item, Module, Project};
use crate::error::ParserError;

use crate::token::{SpecialKeywordKind, Token, TokenKind};

mod expr;
mod item;
mod stmt;
mod ty;

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

type PResult<T> = Result<T, ParserError>;

pub fn get_ast_from_tokens(tokens: Vec<Token>) -> PResult<Project> {
    Parser::from_tokens(tokens).parse()
}
/// Parses a `Project` from a file.
/// It's most likely a temporary function, because we can only parse one file, meaning a file would be the whole project.
/// In the future of course this would not be a thing.
pub fn parse_project_from_file(tokens: Vec<Token>) -> PResult<Project> {
    let mut parser = Parser::from_tokens(tokens);
    parser.parse()
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,

    infix_bp: std::collections::HashMap<String, expr::BindingPower>,
}

/// This impl block is the entry block
impl Parser {
    fn new() -> Self {
        Parser::from_tokens(Vec::new())
    }
    fn from_tokens(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            infix_bp: expr::BindingPower::infix_binding_powers(),
        }
    }
    fn parse_(&mut self) -> PResult<Vec<Item>> {
        let mut items: Vec<Item> = Vec::new();

        while !self.is_at_end() {
            let item = self.parse_item().unwrap();
            items.push(item);
        }
        Ok(items)
    }
    fn parse(&mut self) -> PResult<Project> {
        let mut items: Vec<Item> = Vec::new();
        while !self.is_at_end() {
            let item = self.parse_item().unwrap();
            items.push(item);
        }
        let module = Module { items };
        let project = Project {
            modules: vec![module],
        };
        Ok(project)
    }
}

/// Adjusts the given index by the given offset safely.
///
/// This function takes an index as a `usize` and an offset as an `isize`. It attempts to add or subtract the offset
/// to/from the index based on the sign of the offset. It ensures that the operation is checked, meaning that it will not
/// cause any overflow or underflow.
fn adjust_index_safely(index: usize, offset: isize) -> Option<usize> {
    match offset.is_positive() {
        true => index.checked_add(offset as usize),
        false => index.checked_sub(-offset as usize),
    }
}

/// This impl block is for the cursor functionality
impl Parser {
    fn peek(&self, offset: isize) -> Option<&Token> {
        adjust_index_safely(self.current, offset).and_then(|new_offset| self.tokens.get(new_offset))
    }

    fn check(&self, kind: &TokenKind, offset: isize) -> Result<&Token, Option<&Token>> {
        self.check_with(offset, |tk| tk == kind)
    }
    fn check_with<F>(&self, offset: isize, pred: F) -> Result<&Token, Option<&Token>>
    where
        Self: Sized,
        F: Fn(&TokenKind) -> bool,
    {
        self.peek(offset)
            .map(|c| if pred(&c.kind) { Ok(c) } else { Err(Some(c)) })
            .unwrap_or(Err(None))
    }
    fn eat(&mut self, kind: &TokenKind) -> Result<&Token, Option<&Token>> {
        self.eat_with(|t| t == kind)
    }
    fn eat_identifier(&mut self) -> Result<&Token, Option<&Token>> {
        self.eat_with(TokenKind::is_identifier)
    }
    fn eat_punctuator(&mut self) -> Result<&Token, Option<&Token>> {
        self.eat_with(TokenKind::is_punctuator)
    }
    fn eat_keyword(&mut self) -> Result<&Token, Option<&Token>> {
        self.eat_with(TokenKind::is_keyword)
    }
    fn eat_literal(&mut self) -> Result<&Token, Option<&Token>> {
        self.eat_with(TokenKind::is_literal)
    }
    fn eat_just(&mut self) -> Result<&Token, Option<&Token>> {
        self.eat_with(|_| true)
    }
    fn eat_with<F>(&mut self, pred: F) -> Result<&Token, Option<&Token>>
    where
        Self: Sized,
        F: Fn(&TokenKind) -> bool,
    {
        // This is an ugly workaround for a limitation of the borrow checker.
        // Rust's new polonius would be able to handle this better,
        // but for now we'll just use this.
        // TODO: If polonius is ready, use the better version.

        if let Ok(..) = self.check_with(0, &pred) {
            self.advance()
        }

        match self.peek(-1) {
            Some(token) if pred(&token.kind) => Ok(token),
            peeked @ Some(..) => Err(peeked),
            None => Err(None),
        }

        // // After polonius is ready, this SHOULD work
        // match self.check_with(0, &pred) {
        //     Ok(token) => {
        //         self.advance();
        //         Ok(token)
        //     }
        //     Err(err) => Err(err),
        // }

        // self.check_with(0, &pred).map(|tok| {
        //     self.advance();
        //     tok
        // })
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
