use crate::lexer;
use std::vec::Vec;

struct AST {
    int: i64,
    float: f64,
    string: String,
}

struct Parser {
    input: Vec<lexer::Token>,
    i: usize,
}

