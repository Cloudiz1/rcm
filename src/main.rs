use std::env;
use std::fs;

pub mod lexer;
pub mod parser;
pub mod tac;

// pub mod symbol;
// pub mod types;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        println!("No target to compile.");
        std::process::exit(1);
    }

    let src = fs::read_to_string(args[1].clone()).expect("Could not find target.\n");
    let mut tokenizer = lexer::Tokenizer::new();
    let tokens = tokenizer.tokenize(src);

    let mut parser = parser::Parser::new();
    let Some(ast) = parser.parse(tokens) else {
        return;
    };

    println!("{:#?}", ast);

    let mut tac = tac::TAC::new();
    tac.codegen(ast);
}
