use crate::parser;
use std::collections::HashMap;

pub type BlockId = usize;
pub type ValueId = usize;
const UNDEF_ID: usize = 0;

pub struct SSA {
    blocks: Vec<Block>, // uses BlockId
    values: Vec<Value>, // uses ValueId
    types: Vec<parser::Type>, // uses ValueId
    use_chains: Vec<Vec<ValueId>>,

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

#[derive(Clone)]
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

#[derive(Clone)]
pub enum UnaryOp {
    Not, 
    Neg,
}

#[derive(Clone)]
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
        variable: String,
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
            use_chains: Vec::new(),

            curr_val_id: UNDEF_ID + 1,
            curr_block_id: 0,
        }
    }

    fn add_use(&mut self, operand: ValueId, user: ValueId) {
        self.use_chains[operand].push(user);
    }

    fn add_variable(&mut self, value: Value) -> ValueId {
        self.values.push(value);
        self.curr_val_id += 1;
        return self.curr_val_id - 1;
    }

    fn write_variable(&mut self, variable: String, block: BlockId, value: ValueId) {
        let b = &mut self.blocks[block];
        b.current_defintions.insert(variable, value);
    }

    fn read_variable(&mut self, variable: String, block: BlockId) -> ValueId {
        if let Some(value) = self.blocks[block].current_defintions.get(&variable) {
            return value.clone();
        } 

        self.read_variable_recursive(variable, block)
    }

    fn read_variable_recursive(&mut self, variable: String, block: BlockId) -> ValueId {
        let v: ValueId;
        if !self.blocks[block].sealed {
            let phi = Value::Phi { variable: variable.clone(), block, operands: Vec::new() };
            v = self.add_variable(phi);
            self.blocks[block].incomplete_phis.insert(variable.clone(), v);
        } else if self.blocks[block].predecessors.len() == 1 {
            v = self.read_variable(variable.clone(), self.blocks[block].predecessors[0]);
        } else {
            let phi = Value::Phi { variable: variable.clone(), block, operands: Vec::new() };
            v = self.add_variable(phi.clone());
            self.write_variable(variable.clone(), block, v);
            self.add_phi_operands(variable.clone(), v);
        }

        self.write_variable(variable, block, v);
        return v;
    }

    fn add_phi_operands(&mut self, variable: String, phi_id: ValueId) -> ValueId {
        let block_id = match &self.values[phi_id] {
            Value::Phi { block, .. } => *block,
            _ => panic!("internal error: can not call ir::SSA::add_phi_operands() without a phi variant"),
        };

        let preds = self.blocks[block_id].predecessors.to_owned();
        let mut new_operands: Vec<ValueId> = Vec::new();

        for pred in preds {
            let operand = self.read_variable(variable.clone(), pred);
            self.add_use(operand, phi_id);
            new_operands.push(operand);
        }

        if let Value::Phi { operands, .. } = &mut self.values[phi_id] {
            *operands = new_operands.clone();
        }

        return self.remove_trivial_phi(phi_id);
    }

    fn remove_trivial_phi(&mut self, phi_id: ValueId) -> ValueId {
        let same = {
            let Value::Phi { operands, ..} = &self.values[phi_id] else { return phi_id };
            let mut same: Option<ValueId> = None;
            for &op in operands {
                if Some(op) == same || op == phi_id { continue };
                if same.is_some() { return phi_id };
                same = Some(op);
            }

            same.unwrap_or(UNDEF_ID)
        };

        let users = std::mem::take(&mut self.use_chains[phi_id]);
        for user_id in users {
            if user_id == phi_id { continue }
            self.reroute(user_id, phi_id, same);

            if let Value::Phi { .. } = self.values[user_id] {
                self.remove_trivial_phi(user_id);
            }
        }

        return same;
    }

    fn reroute(&mut self, user_id: ValueId, old: ValueId, new: ValueId) {
        match &mut self.values[user_id] {
            Value::Binary { lhs, rhs, ..} => {
                if *lhs == old { *lhs = new }
                if *rhs == old { *rhs = new }
            },
            Value::Phi { operands, .. } => {
                for op in operands.iter_mut() {
                    if *op == old { *op = new }
                }
            }
            _ => unimplemented!("reroute is still a WIP"),
        }

        self.add_use(new, user_id);
    }

    fn seal_block(&mut self, block_id: BlockId) {
        let incomplete_phis = std::mem::take(&mut self.blocks[block_id].incomplete_phis);
        for (variable, phi) in incomplete_phis {
            self.add_phi_operands(variable, phi);
        }

        self.blocks[block_id].sealed = true;
    }
}
