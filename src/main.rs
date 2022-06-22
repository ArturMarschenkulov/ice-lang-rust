mod ast;
mod compiler;
mod evaluator;
mod parser;
mod token;
mod tokenizer;

use crate::ast::print_ast;
use crate::evaluator::get_evaluation_from_ast;
use crate::parser::get_ast_from_tokens;
use crate::tokenizer::get_tokens_from_source;
use std::fs::File;
use std::io::Read;
use std::time::Instant;
//use crate::compiler::get_asm_from_ast;

static TEXT: &str = r#"
var a = 0;
a == a;
"#;

struct Ice {}
impl Ice {
    fn main(&mut self) {
        let num_arg = std::env::args().count() == 2;
        if num_arg {
            println!("from file");
            let text_from_file = &mut std::env::args().collect::<Vec<String>>()[1];
            self.run_file(text_from_file)
        } else {
            println!("from memory");
            let text_from_memoroy = TEXT;
            self.run(text_from_memoroy);
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
        let show_stages = true;
        let show_token_stream = true;
        let show_ast_tree = true;

        println!("{}", text.to_owned());
        if show_stages {
            println!("    Tokenizer Stage:");
        }
        let now = Instant::now();
        let tokens = get_tokens_from_source(text);
        if show_stages {
            println!(
                "      Tokenizer took {} nanoseconds",
                now.elapsed().as_nanos()
            );
        }
        if show_token_stream {
            for token in &tokens {
                println!("{:?}", token.kind)
            }
        }

        if show_stages {
            println!("    Parser Stage:");
        }
        let now = Instant::now();
        let ast = get_ast_from_tokens(tokens);
        if show_stages {
            println!("      Parser took {} nanoseconds", now.elapsed().as_nanos());
        }
        if show_ast_tree {
            print_ast(&ast);
        }
        if false {
            if show_stages {
                println!("    Evaluator Stage:");
            }
            let now = Instant::now();
            get_evaluation_from_ast(&ast);
            if show_stages {
                println!(
                    "      Evaluator took {} nanoseconds",
                    now.elapsed().as_nanos()
                );
            }
        }

        //get_asm_from_ast(&ast);

        println!("{:?} milliseconds have passed", now.elapsed().as_millis());
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
