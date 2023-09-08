#![allow(dead_code)]
mod compiler;
mod lexer;
mod parser;
mod tui;

use std::fs::File;
use std::io::Read;

//use crate::evaluator::get_evaluation_from_ast;
use crate::lexer::lex_tokens_from_file;
use crate::parser::parse_project_from_file;

//use crate::compiler::get_asm_from_ast;

static TEXT: &str = r#"

// THis is a comment at the end
var stur := "This\n is a string\n";
// THis is a comment at the end
"#;

struct Ice {}
impl Ice {
    // the entry point to the `Ice` instance
    fn main(&mut self) {
        let num_arg = std::env::args().count();
        let source = match num_arg {
            2 => {
                println!("from file");
                let text_from_file = &mut std::env::args().collect::<Vec<String>>()[1];
                Ice::read_file(text_from_file)
            }
            1 => {
                let run_test_file = true;
                if run_test_file {
                    // This is mainly for convenience
                    // The actual end product should not have this branch

                    let file = "tests\\test.ice";
                    Ice::read_file(&mut file.to_owned())
                } else {
                    println!("from memory");
                    String::from(TEXT)
                }
            }
            _ => unreachable!(),
        };
        self.run(&source);
    }
    fn read_file(path: &mut String) -> String {
        let error_string = path.to_string();

        let mut contents = String::new();
        let _num_byte_appended = File::open(path)
            .unwrap_or_else(|_| panic!("{}", error_string))
            .read_to_string(&mut contents)
            .expect("something went wrong reading the file");
        if contents.is_empty() {
            ::std::process::exit(0)
        }
        contents
    }

    fn run(&mut self, text: &str) {
        let mut time_vec = Vec::<(&str, std::time::Duration)>::new();

        tui::print_source_code(text);

        let show_stages = true;
        let show_token_stream = true;
        let show_ast_tree = true;

        let (tokens, lexer_time) = tui::format(
            "Lexer",
            show_stages,
            show_token_stream,
            || lex_tokens_from_file(text),
            tui::print_token_stream,
        );
        time_vec.push(("Lexer", lexer_time));
        let tokens = tokens.unwrap();

        let (_ast, parser_time) = tui::format(
            "Parser",
            show_stages,
            show_ast_tree,
            || parse_project_from_file(tokens.clone()),
            tui::print_ast_tree,
        );
        time_vec.push(("Parser", parser_time));
        let _ast = _ast.unwrap();

        tui::print_time(&time_vec);
    }
}

/// Main entry point to the whole project/compiler
fn main() {
    // std::env::set_var("RUST_BACKTRACE", "1");
    println!("------------------------------------------------------------");
    let mut ice = Ice {};
    ice.main();
    println!("------------------------------------------------------------");
}
