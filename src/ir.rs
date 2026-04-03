use crate::parser;
use std::collections::HashMap;

pub type BlockId = usize;
pub type ValueId = usize;

pub struct SSA {
    blocks: Vec<Block>, // uses BlockId
    values: Vec<Value>, // uses ValueId
    types: Vec<parser::Type>, // uses ValueId

    // track the current id for arenas
    curr_val_id: ValueId,
    curr_block_id: ValueId,
}

pub struct Block {
    pub current_defintions: HashMap<String, ValueId>,
    pub incomplete_phis: HashMap<String, ValueId>,
    pub instructions: Vec<ValueId>,

    pub filled: bool,
    pub sealed: bool,

    pub predecessors: Vec<BlockId>, 
    pub successors: Vec<BlockId>
}

pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    LShift,
    RShift,
    LNot, // LAnd and LOr are handled in the CFG for short circuiting
    GT,
    GTE,
    LT,
    LTE,
    Eq,
    NotEq,
}

pub enum UnaryOp {
    Not, 
    Neg,
}

pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Binary {
        op: BinaryOp,
        lhs: ValueId,
        rhs: ValueId,
    },
    Unary {
        op: UnaryOp,
        member: ValueId,
    },
    GetElmPtr {
        base: ValueId,
        index: ValueId,
        size: usize,
    },
    Load(ValueId),
    Store {
        address: ValueId,
        value: ValueId,
    },
    Call {
        name: String,
        args: Vec<ValueId>,
    },
    Jump(BlockId),
    Branch {
        cond: ValueId,
        if_true: BlockId,
        if_false: BlockId,
    },
    Phi {
        // variable: String,
        block: BlockId,
        operands: Vec<ValueId>,
    }
}

impl SSA {
    pub fn new() -> Self {
        Self {
            blocks: Vec::new(),
            values: Vec::new(),
            types: Vec::new(),

            curr_val_id: 0,
            curr_block_id: 0,
        }
    }

    fn add_variable(&mut self, value: Value) -> ValueId {
        self.values.push(value);
        self.curr_val_id += 1;
        return self.curr_val_id - 1;
    }

    fn write_variable(&mut self, variable: String, block: BlockId, value: ValueId) {
        let b = &mut self.blocks[block];
        if let Some(_) = b.current_defintions.get(&variable) {
            b.current_defintions.insert(variable, value);
        }
    }

    fn read_variable(&mut self, variable: String, block: BlockId) -> ValueId {
        if let Some(value) = self.blocks[block].current_defintions.get(&variable) {
            return value.clone();
        } 

        self.read_variable_recursive(variable, block)
    }

    fn read_variable_recursive(&mut self, variable: String, block: BlockId) -> ValueId {
        let mut v: ValueId;
        if !self.blocks[block].sealed {
            let phi = Value::Phi { block: block, operands: Vec::new() };
            v = self.add_variable(phi);
            self.blocks[block].incomplete_phis.insert(variable, v);
        } else if self.blocks[block].predecessors.len() == 1 {
            v = self.read_variable(variable, self.blocks[block].predecessors[0]);
        } else {
            let phi = Value::Phi { block: block, operands: Vec::new() };
            v = self.add_variable(phi);
            self.write_variable(variable, block, v);
            v = self.add_phi_operands(variable, v);
        }

        self.write_variable(variable, block, v);
        return v;
    }

    fn add_phi_operands(&mut self, variable: String, phi: Value) {
        let Value::Phi { block, operands } = phi else {
            panic!("internal error: can not call ir::SSA::add_phi_operands() without a phi variant");
        };

        for pred in &self.blocks[block].predecessors {
        }
    }
}
