mod ast;
mod compiler;
mod error;
mod evaluator;
mod lexer;
mod parser;
mod token;

use crate::ast::print_ast;
//use crate::evaluator::get_evaluation_from_ast;
use crate::lexer::get_tokens_from_source;
use crate::parser::get_ast_from_tokens;
use std::fs::File;
use std::io::Read;
use std::time::Instant;
//use crate::compiler::get_asm_from_ast;

static TEXT: &str = r#"

// THis is a comment at the end
var stur := "This\n is a string\n";
// THis is a comment at the end
"#;

struct Ice {}
impl Ice {
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
                    let file = "tests\\test.ice";
                    self.run_file(&mut file.to_owned());
                } else {
                    println!("from memory");
                    let text_from_memoroy = TEXT;
                    self.run(text_from_memoroy);
                }
            }
        }
        if num_arg {
            println!("from file");
            let text_from_file = &mut std::env::args().collect::<Vec<String>>()[1];
            self.run_file(text_from_file)
        } else {
            let run_test_file = true;
            if run_test_file {
                let file = "tests\\test.ice";
                self.run_file(&mut file.to_owned());
            } else {
                println!("from memory");
                let text_from_memoroy = TEXT;
                self.run(text_from_memoroy);
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

        println!("");
        println!("Source code start:");
        println!("{}", text.to_owned());
        println!("Source code end.");
        println!("");

        let show_stages = true;
        let show_token_stream = true;
        let show_ast_tree = true;

        let lexer_str = ansi_term::Color::Cyan.paint("Lexer").to_string();
        let parser_str = ansi_term::Color::Cyan.paint("Parser").to_string();
    

        if show_stages {
            println!("");
            println!("{} Stage:", lexer_str);
            println!("");
        }

        let now = Instant::now();
        let tokens = get_tokens_from_source(text);
        time_vec.push(("Lexer", now.elapsed()));

        if show_token_stream {
            for token in &tokens {
                println!("{:?} {:?} {:?}", &token.kind, &token.whitespace, &token.span);
            }
        }

        if show_stages {
            println!("");
            println!("{} Stage:", parser_str);
            println!("");
        }
        
        let now = Instant::now();
        let ast = get_ast_from_tokens(tokens);
        time_vec.push(("Parser", now.elapsed()));

        if show_ast_tree {
            print_ast(&ast);
        }

        for (stage, time) in time_vec {
            let stage = ansi_term::Color::Cyan.paint(stage).to_string();
            let nano_t = ansi_term::Color::Red.paint(format!("{}", time.as_nanos())).to_string();
            let micro_t = ansi_term::Color::Red.paint(format!("{}", time.as_micros())).to_string();
            let mili_t = ansi_term::Color::Red.paint(format!("{}", time.as_millis())).to_string();
            let sec_t = ansi_term::Color::Red.paint(format!("{}", time.as_secs())).to_string();
            println!("{:15} took {:>20}nanosec, {:>20}microsec, {:>20}milisec, {:>20}sec", stage, nano_t, micro_t, mili_t, sec_t);
        }

        println!("{:?} milliseconds have passed", now.elapsed().as_nanos());
    }
}
fn main() {
    let mut ice = Ice {};
    ice.main();
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
