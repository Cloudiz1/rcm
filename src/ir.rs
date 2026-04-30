use crate::lexer;
use crate::parser;
use crate::analysis;
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
    Array {
        elements: Vec<ValueId>,
    },
    ArrayAccess {
        array: ValueId,
        index: ValueId,
    },
    Struct {
        identifier: String,
        members: Vec<ValueId>,
    },
    GetElmPtr {
        base: ValueId,
        index: ValueId,
    },
    Load(ValueId),
    Store {
        address: ValueId,
        value: ValueId,
    },
    Address(ValueId),
    Call {
        name: String,
        args: Vec<ValueId>,
    },
    Phi {
        // variable: String,
        block: BlockId,
        operands: Vec<ValueId>,
    },
    Parameter {
        index: usize,
        t: parser::Type,
    },
    Jump(BlockId),
    Branch {
        cond: ValueId,
    },
    Ret {
        value: ValueId,
    },
    UNDEF,
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
    pub successors: Vec<BlockId>,
}

impl Block {
    fn new(name: &'static str) -> Self {
        Self {
            name,
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

pub struct SSA {
    expression_arena: Vec<parser::Expression>, 

    blocks: Vec<Block>,
    values: Vec<Value>, 
    values_table: HashMap<Value, ValueId>, // for LVN
    use_chains: Vec<Vec<ValueId>>, // for removing trivial phis
    types: Vec<parser::Type>, // uses ValueId

    curr_val_id: ValueId,
    curr_block_id: ValueId,
    pred: Option<BlockId>,

    exit_block: BlockId,
    returns: Vec<ValueId>,
    symbols: HashMap<String, analysis::Symbol>,
}

impl SSA {
    pub fn new(globals: HashMap<String, analysis::Symbol>, expression_arena: Vec<parser::Expression>) -> Self {
        Self {
            expression_arena,

            blocks: Vec::new(),
            values: Vec::new(),
            values_table: HashMap::new(),
            types: Vec::new(),
            use_chains: Vec::new(),

            curr_val_id: 0,
            curr_block_id: 0,
            pred: None,

            exit_block: 0,
            returns: Vec::new(),
            symbols: globals,
        }
    }

    fn add_use(&mut self, operand: ValueId, user: ValueId) {
        if self.use_chains[operand].contains(&user) { return; }
        self.use_chains[operand].push(user)
    }

    fn add_block(&mut self, block: Block) -> BlockId {
        // if !block.predecessors.is_empty() {
        //     self.blocks[block.predecessors[0]].successors.push(self.curr_block_id);
        // }

        self.pred = Some(self.curr_block_id);
        self.blocks.push(block);
        self.curr_block_id += 1;
        return self.curr_block_id - 1;
    }

    fn create_edge(&mut self, pred: BlockId, succ: BlockId) {
        self.blocks[pred].successors.push(succ);
        self.blocks[succ].predecessors.push(pred);
    }

    fn add_inst(&mut self, block: BlockId, val: ValueId) {
        self.blocks[block].instructions.push(val);
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
            let phi = Value::Phi { block, operands: Vec::new() };
            v = self.add_value(phi);
            self.blocks[block].incomplete_phis.insert(variable.clone(), v);
        } else if self.blocks[block].predecessors.len() == 1 {
            v = self.read_variable(variable.clone(), self.blocks[block].predecessors[0]);
        } else {
            let phi = Value::Phi { block, operands: Vec::new() };
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
    // handles dot operators
    fn expr_to_string(&self, expr: parser::ExpressionId) -> String {
        match &self.expression_arena[expr] {
            parser::Expression::Identifier(s) => s.clone(),
            parser::Expression::Dot {
                lhs, rhs
            } => std::format!("{}.{}", self.expr_to_string(*lhs), rhs),
        _ => panic!("internal error: can not convert {:?} to a string", expr),
        }
    }

    /// handles structs and normal variables, instead of just variables
    // fn write_variable_complete(&mut self, identifier: String, val: ValueId) {
    //     if matches!(self.values[val], Value::Struct { .. }) {
    //         self.struct_assignment(identifier, val);
    //     } else {
    //         self.write_variable(identifier, self.pred.unwrap(), val);
    //     }
    // }

    // TODO: rewrite SROA in a later pass
    /// structs need multiple write calls, this function handles that
    // fn struct_assignment(&mut self, variable: String, struct_expr: ValueId) {
    //     // take name, say foo
    //     // iterate through each member, say foo.x and foo.y
    //     // call write variable on them
    //
    //     let updates: Vec<(String, usize)> = {
    //         let Value::Struct { identifier, members } = &self.values[struct_expr] else { 
    //             unreachable!("internal error: called struct_assignment without a struct") 
    //         };
    //
    //         // TODO: use symbol lookup
    //
    //         names.iter().zip(members).map(|(name, member)| {
    //             (name.clone(), *member)
    //         }).collect()
    //     };
    //
    //     for (name, member) in updates {
    //         let var = std::format!("{variable}.{name}");
    //         self.write_variable(var, self.pred.unwrap(), member.clone());
    //     }
    // }

    fn statement(&mut self, stmt: parser::Statement, block_name: &'static str) {
        use parser::Statement;
        match stmt {
            Statement::ParseError => unreachable!("internal error: how did a ParseError even make its way to IR gen"),
            Statement::Block(stmts) => {
                // let b = self.add_block(Block::new(self.pred, block_name));
                let pred = self.pred.unwrap();
                let b = self.add_block(Block::new(block_name));
                self.create_edge(pred, b);
                self.seal_block(b);

                for s in stmts {
                    self.statement(*s, "Basic Block");
                }

                self.blocks[b].filled = true;
            }
            Statement::FunctionDeclaration { 
                name, 
                return_type, 
                parameters, 
                body, 
                public 
            } => {
                self.exit_block = self.add_block(Block::new("exit block"));
                let entry = self.add_block(Block::new("function entry")); // adds param to entry block
                self.seal_block(entry);

                for (i, p) in parameters.into_iter().enumerate() {
                    let parser::Statement::Parameter { name, t } = *p else { unreachable!() };
                    let param = Value::Parameter { index: i, t };
                    let param_id = self.add_value(param);
                    self.write_variable(name, entry, param_id);
                };

                self.blocks[entry].filled = true;
                self.statement(*body, "function body");

                // turns branched returns into a single one
                let returns = std::mem::take(&mut self.returns);
                if returns.len() == 1 {
                    let ret = self.add_value(Value::Ret { value: returns[0] });
                    self.add_inst(self.exit_block, ret);
                } else {
                    let operands = returns.into_iter().collect();
                    let phi = self.add_value(Value::Phi { block: self.pred.unwrap(), operands });
                    let ret = self.add_value(Value::Ret { value: phi });
                    self.add_inst(self.exit_block, ret);
                }

                self.create_edge(self.pred.unwrap(), self.exit_block);
            }
            Statement::ExpressionStatement(expr) => { self.expr(expr); },
            // TODO: pointers
            Statement::VariableDeclaration {
                identifier, 
                variable_type, 
                initial_value, 
                ..
            } => {
                if let Some(e) = initial_value { // if it exists just reuse assignment code
                    let index = self.expression_arena.len();
                    self.expression_arena.push(parser::Expression::Identifier(identifier));
                    self.expression_arena.push(parser::Expression::Assignment { 
                        identifier: index,
                        value: e,
                    });

                    self.expr(index + 1);
                    return;
                }

                let val = self.add_value(Value::UNDEF); // else init with UNDEF
                self.write_variable(identifier, self.pred.unwrap(), val);
            }
            Statement::WhileStatement { 
                condition, 
                block 
            } => {
                let entry = self.add_block(Block::new("while entry"));
                self.create_edge(self.pred.unwrap() - 1, entry);

                self.expr(condition);
                self.blocks[entry].filled = true; // NOT SEALED

                self.statement(*block, "while body"); 
                self.create_edge(self.pred.unwrap(), entry); // loop to while header
                self.seal_block(entry);

                // we need the next block to attach the entry
                self.pred = Some(entry);
                let post = self.add_block(Block::new("use"));
                self.create_edge(entry, post);
                self.seal_block(post);
            }
            Statement::IfStatement { 
                condition, 
                block, 
                alt 
            } => {
                let entry = self.pred.unwrap();
                self.seal_block(entry);

                let cond = self.expr(condition);
                let br = self.add_value(Value::Branch { cond });
                self.add_inst(self.pred.unwrap(), br);
                self.blocks[entry].filled = true;

                self.statement(*block, "then block"); // then
                let then_b = self.pred.unwrap();
                self.blocks[then_b].filled = true;

                let merge_b = self.add_block(Block::new("if merge"));
                self.create_edge(then_b, merge_b);

                if let Some(alt_b) = alt {
                    self.pred = Some(entry); 
                    self.statement(*alt_b, "else block");
                    let else_b = self.pred.unwrap();
                    self.blocks[else_b].filled = true;
                    self.seal_block(else_b);

                    // create the edge between else and merge
                    self.create_edge(else_b, merge_b);
                }

                self.blocks[merge_b].filled = true;
                self.seal_block(merge_b);

                self.pred = Some(merge_b);
            }
            Statement::Parameter { .. } => { unreachable!(); }
            Statement::StructDeclaration { .. } => return, // TODO: methods
            Statement::Member { .. } => return,
            Statement::Return { value } => {
                if let Some(val) = value {
                    let ret = self.expr(val);
                    self.returns.push(ret);
                }

                let jump = self.add_value(Value::Jump(self.exit_block));
                self.add_inst(self.pred.unwrap(), jump);
            }
        }
    }

    fn expr(&mut self, expr: parser::ExpressionId) -> ValueId {
        match self.expression_arena[expr].clone() {
            parser::Expression::Int(i) => self.add_value(Value::Int(i)),
            parser::Expression::Float(f) => self.add_value(Value::Float(HashableFloat(f))),
            parser::Expression::Bool(b) => self.add_value(Value::Bool(b)),
            parser::Expression::Char(c) => self.add_value(Value::Char(c)),
            parser::Expression::Binary { 
                mut lhs, 
                operator, 
                mut rhs, 
            } => {
                // if [lexer::Token::Plus,
                //     lexer::Token::Star,
                //     lexer::Token::Ampersand,
                //     lexer::Token::Pipe,
                //     lexer::Token::Caret
                // ].contains(&operator) {
                //     // TODO: this should be ordered not just swapped????
                //
                //     // std::mem::swap(&mut lhs, &mut rhs);
                // }

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
                
                let new_lhs = self.expr(lhs);
                let new_rhs = self.expr(rhs);
                let val = Value::Binary { op, lhs: new_lhs, rhs: new_rhs };
                let id = self.add_value(val);

                self.add_use(new_lhs, id);
                self.add_use(new_rhs, id);
                return id;
            }
            parser::Expression::Unary { 
                operator, 
                member 
            } => {
                let new_member = self.expr(member);

                match operator {
                    lexer::Token::Ampersand => {
                        let reference = self.add_value(Value::Address(new_member));
                        self.add_use(new_member, reference);
                        return reference;
                    }
                    lexer::Token::DotStar => {
                        let deref = self.add_value(Value::Load(new_member));
                        self.add_use(new_member, deref);
                        return deref;
                    }
                    _ => {}
                };

                let op = match operator {
                    lexer::Token::Bang => UnaryOp::Not,
                    lexer::Token::Minus => UnaryOp::Neg,
                    _ => panic!("internal error: invalid unary operator")
                };

                let val = Value::Unary { op, member: new_member };
                let id = self.add_value(val);
                self.add_use(new_member, id);
                return id;
            }
            parser::Expression::Identifier(name) => {
                self.read_variable(name, self.pred.unwrap())
            }
            parser::Expression::Dot { lhs, rhs } => {
                let base = self.expr(lhs);
                return 0;
            }
            parser::Expression::Assignment { 
                identifier, 
                value 
            } => {
                // Nothing reads the output of Assignment
                // TODO: find candinates for SROA (no direct memory access, not
                // passed as a "reference" to a function, fewer than 16 members)

                // TODO: get rid of that clone pretty please
                if let parser::Expression::Identifier(name) = self.expression_arena[identifier].clone() {
                    let val = self.expr(value);
                    self.write_variable(name.clone(), self.pred.unwrap(), val);
                }
                
                // p.* = foo;
                if matches!(self.expression_arena[identifier], parser::Expression::Unary { operator: lexer::Token::DotStar, .. }) {
                    let address = self.expr(identifier);
                    let val = self.expr(value);
                    let inst = self.add_value(Value::Store { address, value: val });
                    self.add_inst(self.pred.unwrap(), inst);
                    return 0;
                }

                // arr[i] = foo;
                if let parser::Expression::ArrayAccess { identifier, index } = self.expression_arena[identifier] {
                    let access = Value::GetElmPtr { base: self.expr(identifier), index: self.expr(index) };
                    let gep = self.add_value(access);
                    let rhs = self.expr(value);

                    let store = self.add_value(Value::Store {
                        address: gep,
                        value: rhs,
                    });

                    self.add_inst(self.pred.unwrap(), store);
                    return 0;
                };

                // arr = [ .. ]; -> 
                // arr[0] = ..
                // arr[1] = ..
                // ...
                if matches!(self.expression_arena[value], parser::Expression::ArrayConstructor { .. }) { // matches! to save a clone
                    let parser::Expression::ArrayConstructor { values } = self.expression_arena[value].clone() else { unreachable!() };
                    for (i, value) in values.into_iter().enumerate() {
                        // its weird but it this creates a bunch of assignments for an array constructor
                        let index = self.expression_arena.len();
                        self.expression_arena.push(parser::Expression::Int(i as i64));
                        self.expression_arena.push(parser::Expression::ArrayAccess { identifier, index });
                        self.expression_arena.push(parser::Expression::Assignment { identifier: index + 1, value });
                        self.expr(index + 2);
                    }

                    return 0; 
                }

                // let foo = {
                //      x: 1,
                //      y: 2,
                // };
                if matches!(self.expression_arena[value], parser::Expression::StructConstructor { .. }) {
                    let base = self.expr(identifier);
                    let constructor = self.expr(value);

                    let Value::Struct{ identifier: struct_name , members } = self.values[constructor].clone() else { unreachable!() };
                    for (i, member) in members.into_iter().enumerate() {
                        let index = self.add_value(Value::Int (i as i64));
                        let gep = self.add_value( Value::GetElmPtr { base, index });
                        let store = self.add_value(Value::Store { address: gep, value: member });
                        self.add_inst(self.pred.unwrap(), store);
                    }

                    return 0;
                }

                return 0;
            }
            parser::Expression::FunctionCall {
                identifier, args
            } => {
                let name = self.expr_to_string(identifier);
                let args = args.iter().map(|x| self.expr(*x)).collect::<Vec<ValueId>>();
                let call = self.add_value(Value::Call { name, args });
                self.add_inst(self.pred.unwrap(), call);
                return call;
            },
            parser::Expression::ArrayConstructor {
                values
            } => {
                let elements = values.iter().map(|x| self.expr(*x)).collect::<Vec<ValueId>>();
                return self.add_value(Value::Array { elements });
            },
            parser::Expression::ArrayAccess {
                identifier, index 
            } => {
                // TODO: yeah this doesn't work at all with ptr arithmatic but thats not important right now
                // TODO: and no more expr to sring!
                let access = Value::GetElmPtr { base: self.expr(identifier), index: self.expr(index) };
                let inst = self.add_value(access);
                let load = self.add_value(Value::Load(inst));
                return load;
            },
            parser::Expression::StructConstructor {
                identifier, members
            } => {
                let names = {
                    let symbol= self.symbols.get(&identifier).unwrap();
                    let analysis::Symbol::Struct { names, .. } = symbol else { unreachable!() };
                    names.clone()
                }; 

                let new_members = names.iter().map(|name| {
                    let val = members.get(name).unwrap();
                    self.expr(val.clone())
                }).collect(); // done in order of names to preserve order of members

                return self.add_value(Value::Struct { identifier, members: new_members })
            },
            _ => todo!(),
        }
    }

    pub fn ir_gen(&mut self, statements: Vec<parser::Statement>) {
        // let entry = self.add_block(Block::new("entry"));
        // let undef = self.add_value(Value::UNDEF);
        // self.write_variable("@MEMORY".to_owned(), entry, undef);

        for s in statements {
            self.statement(s, "top level");
            self.pred = None;
        }
    }
}

impl SSA {
    pub fn print_ids(&self) {
        println!("///// IDS /////");
        for (i, val) in self.values.iter().enumerate() {
            println!("{}: {:?}: ", i, *val);
        }
        println!("///////////////");
        println!("");
    }

    fn print_instruction(&self, inst: ValueId, mut prev_insts: Vec<ValueId>) {
        if prev_insts.contains(&inst) {
            print!("<{inst}>");
            return;
        }

        prev_insts.push(inst);

        match &self.values[inst] {
            Value::Int(v) => print!("{}", v),
            Value::Float(v) => print!("{}", v),
            Value::Bool(v) => print!("{}", v),
            Value::Char(v) => print!("{}", v),
            Value::String(v) => print!("{}", v),
            Value::Binary { op, lhs, rhs } => {
                print!("(");
                self.print_instruction(*lhs, prev_insts.clone());
                print!(" {:?} ", op);
                self.print_instruction(*rhs, prev_insts);
                print!(")");
            }
            Value::Unary { op, member } => {
                print!("({:?} ", op);
                self.print_instruction(*member, prev_insts);
                print!(")");
            }
            Value::Array { elements } => {
                print!("[");
                for (i, e) in elements.iter().enumerate() {
                    self.print_instruction(*e, prev_insts.clone());
                    if i != elements.len() - 1 { print!(", ") }
                }
                print!("]");
            }
            Value::ArrayAccess { array, index } => {
                print!("Array<{array}>[");
                self.print_instruction(*index, prev_insts);
                print!("]");
            }
            Value::GetElmPtr { base, index } => {
                print!("GEP(");
                prev_insts.push(inst);
                self.print_instruction(*base, prev_insts.clone());
                print!(", ");
                self.print_instruction(*index, prev_insts);
                print!(")");
            },
            Value::Address(val) => {
                print!("addr(");
                self.print_instruction(*val, prev_insts);
                print!(")");
            },
            Value::Load(val) => {
                print!("Load(");
                self.print_instruction(*val, prev_insts);
                print!(")");
            },
            Value::Store { address, value } => {
                print!("Store(");
                self.print_instruction(*address, prev_insts.clone());
                print!(", ");
                self.print_instruction(*value, prev_insts);
                print!(")");
            },
            Value::Call { name, args } => {
                print!("call <{}> (", name);
                for (i, arg) in args.iter().enumerate() {
                    self.print_instruction(*arg, prev_insts.clone());
                    if i != args.len() - 1 { print!(", "); }
                }

                print!(")");
            },
            Value::Jump(block) => print!("JMP <{}>: {}", block, self.blocks[*block].name),
            Value::Phi { block, operands } => {
                print!("phi(");
                for (i, op) in operands.iter().enumerate() {
                    self.print_instruction(*op, prev_insts.clone()); 
                    if i != operands.len() - 1 { print!(", ") };
                }
                print!(")");
            }
            Value::UNDEF => print!("UNDEF"),
            Value::Parameter { index, t } => {
                print!("param({})", index);
            }
            Value::Ret { value } => {
                print!("ret ");
                self.print_instruction(*value, prev_insts);
            }
            Value::Branch { cond } => {
                print!("br <");
                self.print_instruction(*cond, prev_insts);
                print!(">");
            }
            Value::Struct { identifier, members } => {
                print!("struct{{");
                for (i, member) in members.iter().enumerate() {
                    self.print_instruction(member.clone(), prev_insts.clone());
                    if i != members.len() - 1 { print!(", ") }
                }
                print!("}}");
            }
        }
    }

    pub fn print_blocks(&self) {
        for (i, block) in self.blocks.iter().enumerate(){
            println!("{}: {}", i, block.name);
            println!("    instructions:");
            for inst in &block.instructions {
                print!("        ");
                self.print_instruction(*inst, Vec::new());
                println!("");
            }
            println!("    successors: {:?}", block.successors);
            println!("    predecessors: {:?}", block.predecessors);
            println!("");
        }
    }
}
