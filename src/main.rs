#![allow(dead_code)]
mod ast;
mod compiler;
mod error;
mod lexer;
mod parser;
mod token;

use crate::ast::DebugTreePrinter;
//use crate::evaluator::get_evaluation_from_ast;
use crate::lexer::get_tokens_from_source;
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
    let s = vec.clone().iter().map(|(_, t)| t).sum::<std::time::Duration>();
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
        let num_arg = std::env::args().count() == 2;
        match num_arg {
            true => {
                println!("from file");
                let text_from_file = &mut std::env::args().collect::<Vec<String>>()[1];
                self.run_file(text_from_file)
            }
            false => {
                let run_test_file = true;
                if run_test_file {
                    // This is mainly for convenience
                    // The actual end product should not have this branch

                    let file = "tests\\test.ice";
                    self.run_file(&mut file.to_owned());
                } else {
                    println!("from memory");
                    let text_from_memoroy = TEXT;
                    self.run(text_from_memoroy);
                }
            }
        }
    }

    fn run_file(&mut self, path: &mut String) {
        let error_string = path.to_string();

        let mut contents = String::new();
        let _num_byte_appended = File::open(path)
            .unwrap_or_else(|_| panic!("{}", error_string))
            .read_to_string(&mut contents)
            .expect("something went wrong reading the file");
        if contents.is_empty() {
            ::std::process::exit(0)
        }
        self.run(&contents);
    }

    fn run(&mut self, text: &str) {
        let mut time_vec = Vec::<(&str, std::time::Duration)>::new();

        print_source_code(text);

        let show_stages = true;
        let show_token_stream = true;
        let show_ast_tree = true;

        // The lexer part
        let tokens = {
            if show_stages {
                let lexer_str = ansi_term::Color::Cyan.paint("Lexer").to_string();
                print_stage(&lexer_str);
            }
            let now = Instant::now();
            let tokens = get_tokens_from_source(text);
            time_vec.push(("Lexer", now.elapsed()));

            if show_token_stream {
                print_token_stream(&tokens)
            }
            tokens
        };

        // The parser part
        let _ast = {
            if show_stages {
                let parser_str = ansi_term::Color::Cyan.paint("Parser").to_string();
                print_stage(&parser_str);
            }
            let now = Instant::now();
            let ast = parse_project_from_file(tokens);
            time_vec.push(("Parser", now.elapsed()));

            if show_ast_tree {
                ast.print_debug_tree()
            }
            ast
        };

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

/*
   Languages for inspiration:
       C/C++, Rust, Swift, Odin, Zig, Python, Pascal

       compiled, statically typed, strongly typed



       Arrays:
           let arr = [0..n];
           let arr = [1, 2, 3];



       prodtype ProdType {};
       sumtype SumType {};




       var a = 2;
       a = 3; // Error


       Ideas:
           Junction:
               if foo == 1 | 2 | {

               }


*/
