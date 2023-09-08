use crate::lexer;
use crate::parser;
use parser::ast_print::DebugTreePrinter;
use std::time::Instant;

fn time_fn<F, A>(f: F) -> (A, std::time::Duration)
where
    F: FnOnce() -> A,
{
    let now = Instant::now();
    let r = f();
    let e = now.elapsed();
    (r, e)
}

pub fn print_stage(stage: &str) {
    println!();
    println!("{} Stage:", stage);
    println!();
}

pub fn print_time(vec: &Vec<(&str, std::time::Duration)>) {
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

pub fn print_token_stream(tokens: &Vec<lexer::token::Token>) {
    println!("----Token Stream----");
    for token in tokens {
        println!(
            "retat: {:?} {:?} {:?}",
            &token.kind, &token.whitespace, &token.span
        );
    }
    println!("~~~~Token Stream~~~~");
}
pub fn print_ast_tree(ast: &parser::ast::Project) {
    println!("----AST Tree----");
    ast.print_debug_tree();
    println!("~~~~AST Tree~~~~");
}

pub fn print_source_code(source: &str) {
    println!();
    println!("----Source Code----");
    println!("{}", source);
    println!("~~~~Source Code~~~~");
    println!();
}

pub fn format<T, E, F>(
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
    let (res, time) = time_fn(f);
    if show_structure {
        f2(res.as_ref().unwrap())
    }
    (res, time)
}
