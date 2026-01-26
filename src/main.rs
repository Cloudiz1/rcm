use std::env;
use std::fs;

pub mod lexer;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        println!("No target to compile.");
        std::process::exit(1);
    }

    let src = fs::read_to_string(args[1].clone()).expect("Could not find target.\n");
    let mut tokenizer = lexer::Tokenizer::new(); 
    let Some(tokens) = tokenizer.tokenize(src, args[1].clone()) else {
        return;
    };

    lexer::print_tokens(tokens);
}
