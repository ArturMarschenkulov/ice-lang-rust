mod ast;
mod evaluator;
mod tokenizer;
mod token;
mod parser;
mod compiler;


use crate::tokenizer::get_tokens_from_source;
use crate::parser::get_ast_from_tokens;
use crate::evaluator::get_evaluation_from_ast;
use std::fs::File;
use std::io::Read;
use std::time::Instant;
use crate::ast::print_ast;
//use crate::compiler::get_asm_from_ast;


static TEXT : &str
= r#"
var a = 3;
a = 4;
"#;

struct Ice {
}
impl Ice {
    fn main(& mut self) {
        if std::env::args().count() == 2 {
            println!("from file");
            self.run_file(&mut std::env::args().collect::<Vec<String>>()[1])
        } else {
            println!("from memory");
            self.run(&mut String::from(TEXT));
        }
    }

    fn run_file(&mut self,  path: &mut String) {
        //let error_string = format!("{}", &path);
        let error_string = path.to_string();
        //let mut file = File::open(path).expect(error_string.as_str());
        let mut file = File::open(path).unwrap_or_else(|_|{panic!(error_string)});
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .expect("something went wrong reading the file");
        if contents.is_empty() {
            ::std::process::exit(0)
        }
        self.run(&mut contents);
    }

    fn run(&mut self, text: &mut String) {

        let show_stages = true;
        let show_token_stream = true;
        let show_ast_tree = true;

        println!("{}", text.clone());
        if show_stages { println!("    Tokenizer Stage:");}
        let now = Instant::now();
        let tokens = get_tokens_from_source(text);
        if show_stages { println!("      Tokenizer took {} nanoseconds", now.elapsed().as_nanos());}
        if show_token_stream {
            for token in &tokens {
                println!("{:?}", token.kind)
            }
        }

        if show_stages { println!("    Parser Stage:");}
        let now = Instant::now();
        let ast = get_ast_from_tokens(tokens);
        if show_stages { println!("      Parser took {} nanoseconds", now.elapsed().as_nanos());}
        if show_ast_tree {
            print_ast(&ast);
        }
        if false {
            if show_stages { println!("    Evaluator Stage:"); }
            let now = Instant::now();
            get_evaluation_from_ast(&ast);
            if show_stages { println!("      Evaluator took {} nanoseconds", now.elapsed().as_nanos()); }
        }


        //get_asm_from_ast(&ast);



        println!("{:?} milliseconds have passed", now.elapsed().as_millis());

    }
}


fn main() {
    let mut ice = Ice{};
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