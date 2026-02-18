use std::env;
use std::fs;

pub mod util;
pub mod lexer;
pub mod parser;
pub mod analysis;
pub mod ir;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        println!("No target to compile.");
        std::process::exit(1);
    }

    let raw = fs::read_to_string(args[1].clone()).expect("Could not find target.\n");
    // formatted lines for error printing 
    let lines = raw.lines().map(|x| x.trim().to_owned()).collect::<Vec<String>>();
    let mut tokenizer = lexer::Tokenizer::new(lines.clone(), args[1].clone()); 
    let Some(tokens) = tokenizer.tokenize() else {
        return;
    };

    // lexer::print_tokens(tokens.clone());

    let mut parser = parser::Parser::new(tokens, lines, args[1].clone());
    let Some(ast) = parser.parse() else {
        return;
    };

    // dbg!(&ast);

    let mut analyzer = analysis::Analyzer::new();
    analyzer.analyze(ast);

    // println!("{:#?}", ast);
}
