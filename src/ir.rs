use crate::lexer;
use crate::parser;
use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::vec::Vec;

struct SSAGen {
    // current_definition: HashMap<String, Expression>
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

#[derive(Clone)]
struct SSADeclaration {
    lhs: Identifier,
    rhs: Expression,
}

#[derive(Clone)]
struct Block {
    predecessors: Vec<Rc<RefCell<Block>>>,
    instructions: Vec<SSADeclaration>,
    definitions: HashMap<String, Expression>,
    incomplete_phis: Vec<String>,
    sealed: bool,
}

fn write_variable(variable: Identifier, block: &mut Block, value: Expression) {
    block.definitions.insert(variable.name, value);
}

fn read_variable_recursive(variable: Identifier, block: &mut Block) -> Expression {
    let val;
    if !block.sealed {
        val = Expression::Phi { operands: Vec::new() };
        write_variable(variable.clone(), block, val);
        block.incomplete_phis.push(variable.name.clone());
    } else if block.predecessors.len() == 1 {
        // let mut predecessor_rc = Rc::clone(&block.predecessors[0]);
        let predecessor = block.predecessors[0].borrow();
        read_variable(variable, predecessor);
        // val = read_variable(variable, &mut predecessor);
    }
    return Expression::Int(0);
}

fn read_variable(variable: Identifier, block: &mut Block) -> Expression {
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
