use crate::lexer;
use crate::parser;
use std::collections::HashMap;

pub type BlockId = usize;
pub type ValueId = usize;

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

impl std::fmt::Display for HashableFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Block {
    fn new(pred: Option<BlockId>, name: &'static str) -> Self {
        Self {
            name,
            current_definitions: HashMap::new(),
            incomplete_phis: HashMap::new(),
            instructions: Vec::new(),

            filled: false,
            sealed: false,

            predecessors: pred.map_or(Vec::new(), |p| Vec::from([p])),
            successors: Vec::new(),
        }
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
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

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum UnaryOp {
    Not, 
    Neg,
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
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
    Phi {
        variable: String,
        block: BlockId,
        operands: Vec<ValueId>,
    },
    Parameter {
        index: usize,
        t: parser::Type,
    },
    UNDEF,
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

#[derive(Debug, Clone)]
pub struct Block {
    pub name: &'static str,
    pub current_definitions: HashMap<String, ValueId>,
    pub incomplete_phis: HashMap<String, ValueId>,
    pub instructions: Vec<ValueId>,

    pub filled: bool,
    pub sealed: bool,

    pub predecessors: Vec<BlockId>, 
    pub successors: Vec<BlockId>
}

pub struct SSA {
    blocks: Vec<Block>,
    values: Vec<Value>, 
    values_table: HashMap<Value, ValueId>, // for LVN
    use_chains: Vec<Vec<ValueId>>, // for removing trivial phis
    types: Vec<parser::Type>, // uses ValueId

    curr_val_id: ValueId,
    curr_block_id: ValueId,
    pred: Option<BlockId>,
}

impl SSA {
    pub fn new() -> Self {
        Self {
            blocks: Vec::new(),
            values: Vec::new(),
            values_table: HashMap::new(),
            types: Vec::new(),
            use_chains: Vec::new(),

            curr_val_id: 0,
            curr_block_id: 0,
            pred: None,
        }
    }

    fn add_use(&mut self, operand: ValueId, user: ValueId) {
        self.use_chains[operand].push(user)
    }

    fn add_block(&mut self, block: Block) -> BlockId {
        if let Some(p) = self.pred {
            self.blocks[p].successors.push(self.curr_block_id);
        }

        self.pred = Some(self.curr_block_id);
        self.blocks.push(block);
        self.curr_block_id += 1;
        return self.curr_block_id - 1;
    }

    fn add_value(&mut self, value: Value) -> ValueId {
        if let Some(id) = self.values_table.get(&value) { return *id }
        self.values_table.insert(value.clone(), self.curr_val_id);
        self.values.push(value);
        self.use_chains.push(Vec::new());
        self.curr_val_id += 1;
        return self.curr_val_id - 1;
    }

    fn write_variable(&mut self, variable: String, block: BlockId, value: ValueId) {
        let b = &mut self.blocks[block];
        b.current_definitions.insert(variable, value);
    }

    fn read_variable(&mut self, variable: String, block: BlockId) -> ValueId {
        if let Some(value) = self.blocks[block].current_definitions.get(&variable) {
            return value.clone();
        } 

        self.read_variable_recursive(variable, block)
    }

    fn read_variable_recursive(&mut self, variable: String, block: BlockId) -> ValueId {
        let mut v: ValueId;
        if !self.blocks[block].sealed {
            let phi = Value::Phi { variable: variable.clone(), block, operands: Vec::new() };
            v = self.add_value(phi);
            self.blocks[block].incomplete_phis.insert(variable.clone(), v);
        } else if self.blocks[block].predecessors.len() == 1 {
            v = self.read_variable(variable.clone(), self.blocks[block].predecessors[0]);
        } else {
            let phi = Value::Phi { variable: variable.clone(), block, operands: Vec::new() };
            v = self.add_value(phi);
            self.write_variable(variable.clone(), block, v);
            v = self.add_phi_operands(variable.clone(), v);
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

        debug_assert!(new_operands.len() <= self.blocks[block_id].predecessors.len());
        if let Value::Phi { operands, .. } = &mut self.values[phi_id] {
            *operands = new_operands;
        }

        return self.remove_trivial_phi(phi_id);
    }

    fn remove_trivial_phi(&mut self, phi_id: ValueId) -> ValueId {
        let same = {
            let Value::Phi { operands, ..} = &self.values[phi_id] else { panic!("internal error: can not call remove_trivial_phi without phi node") };
            let mut same: Option<ValueId> = None;
            for &op in operands {
                if Some(op) == same || op == phi_id { continue }; // unique or self
                if same.is_some() { return phi_id }; // two values, not trivial
                same = Some(op);
            }

            same.unwrap_or(self.add_value(Value::UNDEF))
        };

        let users = std::mem::take(&mut self.use_chains[phi_id]);
        for user in users {
            if user == phi_id { continue }
            self.reroute(user, phi_id, same);

            if let Value::Phi { .. } = self.values[user] {
                self.remove_trivial_phi(user);
            }
        }

        return same;
    }

    fn reroute(&mut self, user: ValueId, old: ValueId, new: ValueId) {
        match &mut self.values[user] {
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

        self.values_table.remove(&self.values[old]);
        self.use_chains[old].retain(|&x| x != user);
        self.add_use(new, user);
    }

    fn seal_block(&mut self, block_id: BlockId) {
        let incomplete_phis = std::mem::take(&mut self.blocks[block_id].incomplete_phis);
        for (variable, phi) in incomplete_phis {
            self.add_phi_operands(variable, phi);
        }
        
        self.blocks[block_id].sealed = true;
    }
}

// the more 'TAC' like stuff
impl SSA {
    fn statement(&mut self, stmt: parser::Statement, block_name: &'static str) {
        use parser::Statement;
        match stmt {
            Statement::ParseError => panic!("internal error: how did a ParseError even make its way to IR gen"),
            Statement::Block(stmts) => {
                let b = self.add_block(Block::new(self.pred, block_name));
                self.seal_block(b);

                for s in stmts {
                    self.statement(*s, "Basic Block");
                }

                self.blocks[b].filled = true;
            }
            Statement::ExpressionStatement(expr) => { self.expr(expr); },
            Statement::VariableDeclaration {
                identifier, 
                variable_type, 
                initial_value, 
                ..
            } => {
                let val = initial_value.map_or(self.add_value(Value::UNDEF), |e| self.expr(e));
                self.write_variable(identifier, self.curr_block_id - 1, val);
            }
            Statement::FunctionDeclaration { 
                name, 
                return_type, 
                parameters, 
                body, 
                public 
            } => {
                let entry = self.add_block(Block::new(self.pred, "function entry")); // adds param to entry block
                self.seal_block(entry);

                for (i, p) in parameters.into_iter().enumerate() {
                    let parser::Statement::Parameter { name, t } = *p else { unreachable!() };
                    let param = Value::Parameter { index: i, t };
                    let param_id = self.add_value(param);
                    self.write_variable(name, entry, param_id);
                };

                self.blocks[entry].filled = true;
                self.statement(*body, "function body");
            }
            Statement::WhileStatement { 
                condition, 
                block 
            } => {
                let entry = self.add_block(Block::new(self.pred, "while entry"));
                self.expr(condition);
                self.blocks[entry].filled = true; // NOT SEALED

                self.statement(*block, "while body"); 
                self.blocks[entry].predecessors.push(self.pred.unwrap()); // loop to while header
                self.blocks[self.pred.unwrap()].successors.push(entry); // loop to while header
                self.seal_block(entry);

                // we need the next block to attach the entry
                self.pred = Some(entry);
            }
            Statement::IfStatement { 
                condition, 
                block, 
                alt 
            } => {
                let entry = self.add_block(Block::new(self.pred, "if entry"));
                self.seal_block(entry);

                self.expr(condition);
                self.blocks[entry].filled = true;

                self.statement(*block, "then block"); // then
                let then_b = self.pred.unwrap();
                self.blocks[then_b].filled = true;
                // self.seal_block(then_b);

                let merge_b = self.add_block(Block::new(Some(then_b), "if merge"));

                if let Some(alt_b) = alt {
                    self.pred = Some(entry); 
                    self.statement(*alt_b, "else block");
                    let else_b = self.pred.unwrap();
                    self.blocks[else_b].filled = true;
                    self.seal_block(else_b);

                    // create the edge between else and merge
                    self.blocks[else_b].successors.push(merge_b);
                    self.blocks[merge_b].predecessors.push(else_b);
                }

                self.blocks[merge_b].filled = true;
                self.seal_block(merge_b);

                self.pred = Some(merge_b);
            }
            Statement::Parameter { .. } => { unreachable!(); }
            // since these only exist in the frontend, i think this okay?
            Statement::StructDeclaration { .. } => return,
            Statement::Member { .. } => return,
            Statement::Return { value } => {
                // TODO: read variable and return!
                return;
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
                    lexer::Token::LeftCaret => BinaryOp::LT,
                    lexer::Token::LeftCaretEqual => BinaryOp::LTE,
                    lexer::Token::RightCaret => BinaryOp::GT,
                    lexer::Token::RightCaretEqual => BinaryOp::GTE,
                    lexer::Token::EqualEqual => BinaryOp::Eq,
                    lexer::Token::BangEqual => BinaryOp::NotEq,
                    _ => panic!("internal error: invalid operator"),
                };
                
                let new_lhs = self.expr(*lhs);
                let new_rhs = self.expr(*rhs);
                let val = Value::Binary { op, lhs: new_lhs, rhs: new_rhs };
                let id = self.add_value(val);
                self.blocks[self.pred.unwrap()].instructions.push(id);
                // TODO: this can lead to duplicate use chains because of add_value()
                self.add_use(new_lhs, id);
                self.add_use(new_rhs, id);
                return id;
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
                let id = self.add_value(val);
                self.blocks[self.pred.unwrap()].instructions.push(id);
                self.add_use(new_member, id);
                return id;
            }
            parser::Expression::Identifier(name) => {
                self.read_variable(name, self.pred.unwrap())
            }
            e @ parser::Expression::Dot { .. } => {
                self.read_variable(expr_to_string(e), self.pred.unwrap())
            }
            parser::Expression::Assignment { 
                identifier, 
                value 
            } => {
                let val = self.expr(*value);
                self.write_variable(expr_to_string(*identifier), self.pred.unwrap(), val);
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

    pub fn ir_gen(&mut self, statements: Vec<parser::Statement>) {
        for s in statements {
            self.statement(s, "entry");
        }
    }

    fn print_instruction(&self, inst: ValueId, mut prev_phis: Vec<ValueId>) {
        match &self.values[inst] {
            Value::Int(v) => print!("{}", v),
            Value::Float(v) => print!("{}", v),
            Value::Bool(v) => print!("{}", v),
            Value::Char(v) => print!("{}", v),
            Value::String(v) => print!("{}", v),
            Value::Binary { op, lhs, rhs } => {
                print!("(");
                self.print_instruction(*lhs, prev_phis.clone());
                print!(" {:?} ", op);
                self.print_instruction(*rhs, prev_phis);
                print!(")");
            }
            Value::Unary { op, member } => {
                print!("({:?} ", op);
                self.print_instruction(*member, prev_phis);
                print!(")");
            }
            Value::GetElmPtr { base, index, size } => unimplemented!(),
            Value::Load(_) => unimplemented!(),
            Value::Store { address, value } => unimplemented!(),
            Value::Call { name, args } => unimplemented!(),
            Value::Jump(_) => unimplemented!(),
            Value::Phi { variable, block, operands } => {
                print!("phi(");
                for (i, op) in operands.iter().enumerate() {
                    if prev_phis.contains(op) { print!("ITSELF") }
                    else { 
                        prev_phis.push(*op);
                        self.print_instruction(*op, prev_phis.clone()); 
                    }
                    if i != operands.len() - 1 { print!(", ") };
                }
                print!(")");
            }
            Value::UNDEF => print!("UNDEF"),
            Value::Parameter { index, t } => {
                print!("param: ");
                self.print_instruction(*index, prev_phis);
            }
        }
    }

    pub fn print_blocks(&self, cdef: bool) {
        for (i, block) in self.blocks.iter().enumerate(){
            println!("{}: {}", i, block.name);
            println!("    instructions:");
            for inst in &block.instructions {
                print!("\t");
                self.print_instruction(*inst, Vec::new());
                println!("");
            }

            println!("    successors: {:?}", block.successors);
            println!("    predecessors: {:?}", block.predecessors);

            if cdef {
                println!("    cdefs:");
                for (var, val) in &block.current_definitions {
                    print!("    {}: ", var);
                    self.print_instruction(*val, Vec::new());
                    println!("");
                }
            }

            println!("");
        }
    }
}
