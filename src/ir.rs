use crate::lexer;
use crate::parser;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::vec::Vec;

struct SSAGen {
    blocks: Vec<Block>
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
    successors: Vec<Rc<RefCell<Block>>>,

    // predecessors: Vec<usize>,
    // successors: Vec<usize>,
    // instructions: Vec<SSADeclaration>,
    // TODO: i seem to need to hold a current definitions as well
    // because LVN should return an identifier in most cases
    
    // definitions: HashMap<String, Expression>,
    definitions: HashMap<String, usize>,
    incomplete_phis: Vec<Expression>,
    sealed: bool,
}



impl SSAGen {
    pub fn new() -> Self {
        Self {
            blocks: Vec::new(),
        }
    }
} 

fn update_variant(variable: &String, block: &mut Block) -> usize {
    let Some(variant) = block.definitions.get(variable) else {
        unreachable!("could not find a defininition for {}", *variable);
    };

    let new = variant + 1;
    block.definitions.insert(variable.clone(), new);
    return new;
}

fn read_variable(variable: &String, block: &mut Block) -> usize {
    if let Some(current) = block.definitions.get(variable) {
        return current.clone();
    }

    return read_variable_recursive(variable, block);
}

fn read_variable_recursive(variable: &String, block: &mut Block) -> usize {
    if !block.sealed {
        let val = Expression::Phi { operands: Vec::new() };
        let out = update_variant(variable, block);
        block.incomplete_phis.push(val);
        return out;
    } else if block.predecessors.len() == 1 {
        let mut predecessor = (&block.predecessors[0]).borrow_mut();
        return read_variable(variable, &mut *predecessor);
    } else {
        let mut phi = Expression::Phi { operands: Vec::new() };
        let out = update_variant(variable, block);
        add_phi_operands(variable, &mut phi, block);
        return out;
    }
}

fn add_phi_operands(variable: &String, phi: &mut Expression, block: &Block) {
    let Expression::Phi { operands } = phi else {
        unreachable!("RCM internal: can not call IR:add_phi_operands with a non-phi block input");
    };

    for predecessor in block.predecessors.iter() {
        let mut pred = predecessor.borrow_mut();
        let variant = read_variable(variable, &mut *pred);
        operands.push(Identifier {
            name: variable.clone(),
            variant
        });
    }
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
