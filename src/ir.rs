use crate::lexer;
use crate::parser;
use std::collections::HashMap;
use std::rc::Rc;
use std::vec::Vec;

struct SSAGen {
    current_definition: HashMap<String, Expression>
}

#[derive(Clone)]
struct Identifier {
    name: String,
    variant: usize,
}

#[derive(Clone)]
enum Expression {
    Binary {
        lhs: Identifier,
        rhs: Identifier,
        operator: lexer::Token
    },
    Unary {
        operand: Identifier,
        operator: lexer::Token,
    },
    Phi {
        operands: Vec<Identifier>,
    },
    Int(i128),
    Float(f64),
    Identifier(Identifier)
}

struct SSADeclaration {
    lhs: Identifier,
    rhs: Expression,
}

struct Block {
    predecessors: Vec<Rc<Block>>,
    instructions: Vec<SSADeclaration>,
    definitions: HashMap<String, Expression>,
    sealed: bool,
}

fn read_variable_recursive(variable: Identifier, block: Block) -> Expression {
    return Expression::Int(0);
}

fn read_variable(variable: Identifier, block: Block) -> Expression {
    if let Some(current) = block.definitions.get(&variable.name) {
        return current.clone();
    }

    return read_variable_recursive(variable, block);
}

pub fn generate_basic_block(block: parser::Statement) {
    match block {
        parser::Statement::Block(statements) => {

            // not *entirely* sure what to do yet
        }
        parser::Statement::WhileStatement { 
            condition, 
            block 
        } => {
            // generate the necessary CFG
        }
        parser::Statement::IfStatement { 
            condition, 
            block, 
            alt 
        } => {
            // generate all the necessary CFG
        }
        _ => {
            // actually generate the basic blocks 
        }
    }
}
