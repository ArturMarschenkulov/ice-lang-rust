pub mod ast;
pub mod ast_print;
mod cursor;
mod error;
mod expr;
mod item;
mod stmt;
mod ty;
use error::Error;

use super::lexer::token::Token;
use ast::{Item, Module, Project};

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

type PResult<T> = Result<T, Error>;

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
    fn from_tokens(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            infix_bp: expr::BindingPower::infix_binding_powers(),
        }
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
