#![allow(dead_code)]
mod ast;
mod compiler;
mod error;
mod lexer;
mod parser;
mod token;

use crate::ast::DebugTreePrinter;
//use crate::evaluator::get_evaluation_from_ast;
use crate::lexer::lex_tokens_from_file;
use crate::parser::parse_project_from_file;
use std::fs::File;
use std::io::Read;
use std::time::Instant;
//use crate::compiler::get_asm_from_ast;

static TEXT: &str = r#"

// THis is a comment at the end
var stur := "This\n is a string\n";
// THis is a comment at the end
"#;

fn print_stage(stage: &str) {
    println!();
    println!("{} Stage:", stage);
    println!();
}

fn print_time(vec: &Vec<(&str, std::time::Duration)>) {
    let ansi_cyan = ansi_term::Color::Cyan;
    let ansi_red = ansi_term::Color::Red;

    for (stage, time) in vec {
        let stage = ansi_cyan.paint(*stage).to_string();

        let nano_t = ansi_red.paint(format!("{}", time.as_nanos())).to_string();
        let micro_t = ansi_red.paint(format!("{}", time.as_micros())).to_string();
        let mili_t = ansi_red.paint(format!("{}", time.as_millis())).to_string();
        let sec_t = ansi_red.paint(format!("{}", time.as_secs())).to_string();

        println!(
            "{:15} took {:>17}nanosec| {:>17}microsec| {:>17}milisec| {:>17}sec",
            stage, nano_t, micro_t, mili_t, sec_t
        );
    }
    let s = vec
        .clone()
        .iter()
        .map(|(_, t)| t)
        .sum::<std::time::Duration>();
    let stage = ansi_cyan.paint("all").to_string();
    let nano_t = ansi_red.paint(format!("{}", s.as_nanos())).to_string();
    let micro_t = ansi_red.paint(format!("{}", s.as_micros())).to_string();
    let mili_t = ansi_red.paint(format!("{}", s.as_millis())).to_string();
    let sec_t = ansi_red.paint(format!("{}", s.as_secs())).to_string();

    println!(
        "{:15} took {:>17}nanosec| {:>17}microsec| {:>17}milisec| {:>17}sec",
        stage, nano_t, micro_t, mili_t, sec_t
    );
}
fn print_token_stream(tokens: &Vec<token::Token>) {
    println!("----Token Stream----");
    for token in tokens {
        println!(
            "retat: {:?} {:?} {:?}",
            &token.kind, &token.whitespace, &token.span
        );
    }
    println!("~~~~Token Stream~~~~");
}
fn print_source_code(source: &str) {
    println!();
    println!("----Source Code----");
    println!("{}", source);
    println!("~~~~Source Code~~~~");
    println!();
}
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
        String::from(contents)
    }
    fn format<T, E, F>(
        name: &str,
        show_stage: bool,
        show_structure: bool,
        f: F,
        f2: fn(&T) -> (),
    ) -> (Result<T, E>, std::time::Duration)
    where
        F: Fn() -> Result<T, E>,
        E: std::fmt::Debug,
    {
        if show_stage {
            let lexer_str = ansi_term::Color::Cyan.paint(name).to_string();
            print_stage(&lexer_str);
        }
        let now = Instant::now();
        let res = f();
        let time = now.elapsed();

        if show_structure {
            f2(&res.as_ref().unwrap())
        }
        (res, time)
    }
    fn run(&mut self, text: &str) {
        let mut time_vec = Vec::<(&str, std::time::Duration)>::new();

        print_source_code(text);

        let show_stages = true;
        let show_token_stream = true;
        let show_ast_tree = true;

        let (tokens, lexer_time) = Ice::format(
            "Lexer",
            show_stages,
            show_token_stream,
            || lex_tokens_from_file(text),
            print_token_stream,
        );
        time_vec.push(("Lexer", lexer_time));
        let tokens = tokens.unwrap();

        let (_ast, parser_time) = Ice::format(
            "Parser",
            show_stages,
            show_ast_tree,
            || parse_project_from_file(tokens.clone()),
            ast::Project::print_debug_tree,
        );
        time_vec.push(("Parser", parser_time));
        let _ast = _ast.unwrap();

        print_time(&time_vec);

        //println!("{:?} milliseconds have passed", now.elapsed().as_nanos());
    }
}

/// Main entry point to the whole project/compiler
fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");
    println!("------------------------------------------------------------");
    let mut ice = Ice {};
    ice.main();
    println!("------------------------------------------------------------");
}