use std::env;
use std::fs;

pub mod lexer;
pub mod parser;
pub mod analysis;
pub mod ir;
// pub mod tac;

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

    dbg!(&ast);

    let mut analyzer = analysis::Analyzer::new();
    analyzer.analyze(ast.clone());

    let mut codegen = ir::Generator::new();
    let tac = codegen.generate(ast);
    println!("{}", tac);
}
