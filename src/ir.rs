use crate::lexer;
use crate::parser;
use std::collections::HashMap;

pub type BlockId = usize;
pub type ValueId = usize;
const UNDEF_ID: usize = 0;

#[derive(Copy, Clone, Debug)]
pub struct HashableFloat(f64);

impl PartialEq for HashableFloat {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_bits() == other.0.to_bits()
    }
}

impl Eq for HashableFloat {}

impl std::hash::Hash for HashableFloat {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
     } 
}

pub struct SSA {
    blocks_arena: Vec<Block>,
    values_arena: Vec<Value>, 
    values_table: HashMap<Value, ValueId>, // for LVN
    use_chains: Vec<Vec<ValueId>>, // for removing trivial phis
    types: Vec<parser::Type>, // uses ValueId

    // track the current id for arenas
    curr_val_id: ValueId,
    curr_block_id: ValueId,
}

pub struct Block {
    pub current_definitions: HashMap<String, ValueId>,
    pub incomplete_phis: HashMap<String, ValueId>,
    pub instructions: Vec<ValueId>,

    pub filled: bool,
    pub sealed: bool,

    pub predecessors: Vec<BlockId>, 
    pub successors: Vec<BlockId>
}

impl Block {
    fn new() -> Self {
        Self {
            current_definitions: HashMap::new(),
            incomplete_phis: HashMap::new(),
            instructions: Vec::new(),

            filled: false,
            sealed: false,

            predecessors: Vec::new(),
            successors: Vec::new(),
        }
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
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

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum UnaryOp {
    Not, 
    Neg,
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum Value {
    Int(i64),
    Float(HashableFloat),
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
    // Branch {
    //     cond: ValueId,
    //     if_true: BlockId,
    //     if_false: BlockId,
    // },
    Phi {
        variable: String,
        block: BlockId,
        operands: Vec<ValueId>,
    }
}

// handles dot operators
fn expr_to_string(expr: parser::Expression) -> String {
    match expr {
        parser::Expression::Identifier(s) => s,
        parser::Expression::Dot {
            lhs, rhs
        } => std::format!("{}.{}", expr_to_string(*lhs), rhs),
       _ => panic!("internal error: can not convert {:?} to a string", expr),
    }
}

impl SSA {
    pub fn new() -> Self {
        Self {
            blocks_arena: Vec::new(),
            values_arena: Vec::new(),
            values_table: HashMap::new(),
            types: Vec::new(),
            use_chains: Vec::new(),

            curr_val_id: UNDEF_ID + 1,
            curr_block_id: 0,
        }
    }

    fn add_use(&mut self, operand: ValueId, user: ValueId) {
        self.use_chains[operand].push(user);
    }

    fn add_block(&mut self, block: Block) -> BlockId {
        self.blocks_arena.push(block);
        self.curr_block_id += 1;
        return self.curr_block_id - 1;
    }

    fn add_value(&mut self, value: Value) -> ValueId {
        if let Some(id) = self.values_table.get(&value) { return *id }
        self.values_table.insert(value.clone(), self.curr_val_id);
        self.values_arena.push(value);
        self.curr_val_id += 1;
        return self.curr_val_id - 1;
    }

    fn write_variable(&mut self, variable: String, block: BlockId, value: ValueId) {
        let b = &mut self.blocks_arena[block];
        b.current_definitions.insert(variable, value);
    }

    fn read_variable(&mut self, variable: String, block: BlockId) -> ValueId {
        if let Some(value) = self.blocks_arena[block].current_definitions.get(&variable) {
            return value.clone();
        } 

        self.read_variable_recursive(variable, block)
    }

    fn read_variable_recursive(&mut self, variable: String, block: BlockId) -> ValueId {
        let v: ValueId;
        if !self.blocks_arena[block].sealed {
            let phi = Value::Phi { variable: variable.clone(), block, operands: Vec::new() };
            v = self.add_value(phi);
            self.blocks_arena[block].incomplete_phis.insert(variable.clone(), v);
        } else if self.blocks_arena[block].predecessors.len() == 1 {
            v = self.read_variable(variable.clone(), self.blocks_arena[block].predecessors[0]);
        } else {
            let phi = Value::Phi { variable: variable.clone(), block, operands: Vec::new() };
            v = self.add_value(phi.clone());
            self.write_variable(variable.clone(), block, v);
            self.add_phi_operands(variable.clone(), v);
        }

        self.write_variable(variable, block, v);
        return v;
    }

    fn add_phi_operands(&mut self, variable: String, phi_id: ValueId) -> ValueId {
        let block_id = match &self.values_arena[phi_id] {
            Value::Phi { block, .. } => *block,
            _ => panic!("internal error: can not call ir::SSA::add_phi_operands() without a phi variant"),
        };

        let preds = self.blocks_arena[block_id].predecessors.to_owned();
        let mut new_operands: Vec<ValueId> = Vec::new();

        for pred in preds {
            let operand = self.read_variable(variable.clone(), pred);
            self.add_use(operand, phi_id);
            new_operands.push(operand);
        }

        if let Value::Phi { operands, .. } = &mut self.values_arena[phi_id] {
            *operands = new_operands.clone();
        }

        return self.remove_trivial_phi(phi_id);
    }

    fn remove_trivial_phi(&mut self, phi_id: ValueId) -> ValueId {
        let same = {
            let Value::Phi { operands, ..} = &self.values_arena[phi_id] else { return phi_id };
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

            if let Value::Phi { .. } = self.values_arena[user_id] {
                self.remove_trivial_phi(user_id);
            }
        }

        return same;
    }

    fn reroute(&mut self, user_id: ValueId, old: ValueId, new: ValueId) {
        match &mut self.values_arena[user_id] {
            Value::Binary { lhs, rhs, ..} => {
                if *lhs == old { *lhs = new }
                if *rhs == old { *rhs = new }
            },
            Value::Unary { member, .. } => {
                if *member == old { *member = new }
            }
            Value::Phi { operands, .. } => {
                for op in operands.iter_mut() {
                    if *op == old { *op = new; }
                }
            }
            _ => unimplemented!("reroute is still a WIP"),
        }

        self.add_use(new, user_id);
    }

    fn seal_block(&mut self, block_id: BlockId) {
        let incomplete_phis = std::mem::take(&mut self.blocks_arena[block_id].incomplete_phis);
        for (variable, phi) in incomplete_phis {
            self.add_phi_operands(variable, phi);
        }

        self.blocks_arena[block_id].sealed = true;
    }
}

// the more 'TAC' like stuff
impl SSA {
    fn statement(&mut self, stmt: parser::Statement, pred: Option<BlockId>) {
        use parser::Statement;
        match stmt {
            Statement::Block(stmts) => {
                let mut b = Block::new();
                if let Some(p) = pred { b.predecessors.push(p); }
                let b_id = self.add_block(b);

                for s in stmts {
                    self.statement(*s, Some(b_id));
                }
            }
            Statement::ParseError => panic!("internal error: how did a ParseError even make its way to IR gen"),
            // TODO: get current block id
            Statement::ExpressionStatement(expr) => { self.expr(expr); },
            Statement::VariableDeclaration {
                identifier, 
                variable_type, 
                initial_value, 
                ..
            } => {
                self.write_variable(identifier,)
            }
        }
    }

    fn expr(&mut self, expr: parser::Expression) -> ValueId {
        match expr {
            parser::Expression::Int(i) => self.add_value(Value::Int(i)),
            parser::Expression::Float(f) => self.add_value(Value::Float(HashableFloat(f))),
            parser::Expression::Bool(b) => self.add_value(Value::Bool(b)),
            parser::Expression::Char(c) => self.add_value(Value::Char(c)),
            parser::Expression::Binary { 
                mut lhs, 
                operator, 
                mut rhs, 
            } => {
                if [lexer::Token::Plus,
                    lexer::Token::Star,
                    lexer::Token::Ampersand,
                    lexer::Token::Pipe,
                    lexer::Token::Caret
                ].contains(&operator) {
                    std::mem::swap(&mut lhs, &mut rhs);
                }

                let op = match operator {
                    lexer::Token::Plus => BinaryOp::Add,
                    lexer::Token::Minus => BinaryOp::Sub,
                    lexer::Token::Star => BinaryOp::Mul,
                    lexer::Token::Slash => BinaryOp::Div,
                    lexer::Token::Percent => BinaryOp::Mod,
                    lexer::Token::Pipe => BinaryOp::Or,
                    lexer::Token::Caret => BinaryOp::Xor,
                    lexer::Token::DoubleLeftCaret => BinaryOp::LShift,
                    lexer::Token::DoubleRightCaret => BinaryOp::RShift,
                    lexer::Token::Bang => BinaryOp::LNot,
                    lexer::Token::LeftCaret => BinaryOp::GT,
                    lexer::Token::LeftCaretEqual => BinaryOp::GTE,
                    lexer::Token::RightCaret => BinaryOp::LT,
                    lexer::Token::RightCaretEqual => BinaryOp::LTE,
                    lexer::Token::EqualEqual => BinaryOp::Eq,
                    lexer::Token::BangEqual => BinaryOp::NotEq,
                    _ => panic!("internal error: invalid operator"),
                };
                
                let new_lhs = self.expr(*lhs);
                let new_rhs = self.expr(*rhs);
                let val = Value::Binary { op, lhs: new_lhs, rhs: new_rhs };
                self.add_value(val)
            }
            parser::Expression::Unary { 
                operator, 
                member 
            } => {
                let op = match operator {
                    lexer::Token::Bang => UnaryOp::Not,
                    lexer::Token::Minus => UnaryOp::Neg,
                    _ => panic!("internal error: invalid unary operator")
                };

                let new_member = self.expr(*member);
                let val = Value::Unary { op, member: new_member };
                self.add_value(val)
            }
            parser::Expression::Identifier(name) => {
                // TODO: Current BlockId
                self.read_variable(name, 0)
            }
            e @ parser::Expression::Dot { .. } => {
                // TODO: Current BlockId
                self.read_variable(expr_to_string(e), 0)
            }
            parser::Expression::Assignment { 
                identifier, 
                value 
            } => {
                // TODO: Current BlockId
                let val = self.expr(*value);
                self.write_variable(expr_to_string(*identifier), 0, val);
                return val;
            }
            parser::Expression::FunctionCall {
                identifier, args
            } => todo!(),
            parser::Expression::ArrayAccess {
                identifier, index 
            } => todo!(),
            parser::Expression::ArrayConstructor {
                values
            } => todo!(),
            parser::Expression::StructConstructor {
                identifier, members
            } => todo!(),
            _ => todo!(),
        }
    }
}
