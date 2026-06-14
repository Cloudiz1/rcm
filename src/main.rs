use std::env;
use std::fs;

pub mod util;
pub mod lexer;
pub mod parser;
pub mod analysis;
pub mod ssa;
pub mod ralloc;
pub mod benchmark;

fn main() {
    let mut benchmarks = benchmark::Benchmarks::new();

    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        println!("No target to compile.");
        std::process::exit(1);
    }

    benchmarks.add("lexer");
    let raw = fs::read_to_string(args[1].clone()).expect("Could not find target.\n");
    let lines = raw.lines().map(|x| x.trim().to_owned()).collect::<Vec<String>>();
    let mut tokenizer = lexer::Tokenizer::new(lines.clone(), args[1].clone()); 
    let Some(tokens) = tokenizer.tokenize() else {
        return;
    };
    benchmarks.end();

    // lexer::print_tokens(tokens.clone());

    benchmarks.add("parser");
    let mut parser = parser::Parser::new(tokens, lines, args[1].clone());
    let Some((ast, mut expression_arena)) = parser.parse() else {
        return;
    };
    benchmarks.end();

    benchmarks.add("analyzer");
    let mut analyzer = analysis::Analyzer::new(&mut expression_arena);
    let (globals, types) = analyzer.analyze(&ast);
    benchmarks.end();

    // dbg!(&types);
    // parser::print_ast(&ast);
    // parser::print_expressions(&expression_arena);

    benchmarks.add("SSA");
    let mut ssa = ssa::SSAGen::new(globals, expression_arena, types);
    let ir = ssa.ir_gen(ast);
    benchmarks.end();

    ssa::print_ids(&ir);
    ssa::print_blocks(&ir);
    ssa::print_misc(&ir);

    // TODO: do more than just a traversal lol
    // let mut ralloc = ralloc::Postorder::new(&ir);
    // dbg!(ralloc.postorder());

    benchmarks.add("ralloc");
    let dom_tree = ralloc::create_dom_tree(&ir);
    benchmarks.end();

    benchmarks.show();
}
