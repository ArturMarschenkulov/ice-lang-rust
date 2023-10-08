// #![allow(dead_code)]
mod compiler;
mod lexer;
mod parser;
mod tui;

use std::fs;
use std::io;

//use crate::evaluator::get_evaluation_from_ast;
use crate::lexer::lex_tokens_from_file;
use crate::parser::parse_project_from_file;

//use crate::compiler::get_asm_from_ast;

static TEXT: &str = r#"

// THis is a comment at the end
var stur := "This\n is a string\n";
// THis is a comment at the end
"#;

fn time_fn<F, A>(f: F) -> (A, std::time::Duration)
where
    F: FnOnce() -> A,
{
    use std::time::Instant;
    let start_time = Instant::now();
    let result = f();
    let elapsed_time = start_time.elapsed();
    (result, elapsed_time)
}

pub fn read_file(path: &str) -> io::Result<String> {
    fs::read_to_string(path).and_then(|contents| {
        if contents.is_empty() {
            Err(io::Error::new(io::ErrorKind::InvalidData, "File is empty"))
        } else {
            Ok(contents)
        }
    })
}

struct Ice;
impl Ice {
    // the entry point to the `Ice` instance
    fn main(&mut self) {
        let num_arg = std::env::args().count();
        let source = match num_arg {
            2 => {
                println!("from file");
                let text_from_file = &mut std::env::args().collect::<Vec<String>>()[1];
                read_file(text_from_file)
            }
            1 => {
                let run_test_file = true;
                if run_test_file {
                    // This is mainly for convenience
                    // The actual end product should not have this branch

                    let file = "tests\\test.ice";
                    read_file(file)
                } else {
                    println!("from memory");
                    Ok(TEXT.to_string())
                }
            }
            _ => unreachable!(),
        }
        .unwrap_or_else(|e| panic!("{}", e));
        self.run(&source);
    }

    fn run(&mut self, text: &str) {
        let mut time_vec = Vec::<(&str, std::time::Duration)>::new();

        struct PrintConfig {
            show_stages: bool,
            show_token_stream: bool,
            show_ast_tree: bool,
        }
        let print_config = PrintConfig {
            show_stages: true,
            show_token_stream: true,
            show_ast_tree: true,
        };

        let (token_stream, token_stream_time) = time_fn(|| lex_tokens_from_file(text));

        tui::print_source_code(text);
        time_vec.push(("Lexer", token_stream_time));
        tui::format_(
            "Lexer",
            print_config.show_stages,
            print_config.show_token_stream,
            token_stream.clone(),
            tui::print_token_stream,
        );
        let (ast_tree, ast_tree_time) =
            time_fn(|| parse_project_from_file(token_stream.clone().unwrap()));
        time_vec.push(("Parser", ast_tree_time));

        tui::format_(
            "Parser",
            print_config.show_stages,
            print_config.show_ast_tree,
            ast_tree,
            tui::print_ast_tree,
        );

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
