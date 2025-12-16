use std::env;
use std::fs;

pub mod lexer;
pub mod parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        println!("No target to compile.");
        std::process::exit(1);
    }

    let src = fs::read_to_string(args[1].clone()).expect("Could not find target.\n");
    let mut tokenizer = lexer::Tokenizer::new();
    let tokens = tokenizer.tokenize(src);

    lexer::print_tokens(tokens);
}
