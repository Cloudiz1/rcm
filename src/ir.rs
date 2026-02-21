use crate::lexer;
use crate::parser;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::vec::Vec;

struct SSAGen {
    blocks: Vec<Block>,
    t_counter: usize,
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
    Identifier(Identifier),
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
}

#[derive(Clone)]
enum Statement {
    Declaration {
        lhs: Identifier,
        rhs: Expression,
    }
}

#[derive(Clone)]
struct Block {
    instructions: Vec<Statement>,

    predecessors: Vec<Rc<RefCell<Block>>>,
    successors: Vec<Rc<RefCell<Block>>>,

    definitions: HashMap<String, usize>,
    incomplete_phis: Vec<Expression>,
    sealed: bool,
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

impl SSAGen {
    pub fn new() -> Self {
        Self {
            blocks: Vec::new(),
            t_counter: 0,
        }
    }

    fn add_tmp_var(&mut self, expr: Expression, block: &mut Block) -> Identifier {
        let variable = Identifier {
            name: "t".to_owned(),
            variant: self.t_counter,
        };

        let decl = Statement::Declaration {
            lhs: variable.clone(),
            rhs: expr,
        };

        block.instructions.push(decl);
        self.t_counter += 1;

        return variable;
    } 

    fn expression_SSA(&mut self, expr: parser::Expression, block: &mut Block) -> Identifier {
        match expr {
            parser::Expression::Int(val) => {
                return self.add_tmp_var(
                    Expression::Int(val),
                    block
                );
            }
            parser::Expression::Float(val) => {
                return self.add_tmp_var(
                    Expression::Float(val),
                    block
                );
            }
            parser::Expression::Bool(val) => {
                return self.add_tmp_var(
                    Expression::Bool(val),
                    block
                );
            }
            parser::Expression::Char(val) => {
                return self.add_tmp_var(
                    Expression::Char(val),
                    block
                );
            }
            parser::Expression::String(val) => {
                return self.add_tmp_var(
                    Expression::String(val),
                    block
                );
            }
            parser::Expression::Identifier(val) => {
                let variant = read_variable(&val, block);
                return Identifier {
                    name: val,
                    variant
                };
            }
            parser::Expression::Binary {
                lhs,
                operator,
                rhs,
            } => {
                let lval = self.expression_SSA(*lhs, block);
                let rval = self.expression_SSA(*rhs, block);
                let val = Expression::Binary {
                    lhs: lval,
                    rhs: rval,
                    operator
                };

                return self.add_tmp_var(val, block);
            }
            parser::Expression::Unary {
                member,
                operator,
            } => {
                let operand = self.expression_SSA(*member, block);
                let val = Expression::Unary { operand, operator };
                return self.add_tmp_var(val, block);
            }
            parser::Expression::Dot {
                lhs,
                rhs,
            } => {
                // TODO: remember the implicit ptr deref
                todo!();
            }
            parser::Expression::Assignment {
                identifier,
                value
            } => {
                // honestly i dont have a clue what im supposed to do here
                // let lhs = self.expression_SSA(*identifier, block);
                // let rhs = self.expression_SSA(*value, block);
                // let variant = update_variant(&lhs.name, block);
                // let decl = Statement::Declaration {
                //     lhs: Identifier {
                //         name: 
                //     }
                // };
                todo!();
            }
        }
    }

    fn statement_SSA(statement: parser::Statement) {
        match statement {
            parser::Statement::ExpressionStatement(expr) => {
            }
        }
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
