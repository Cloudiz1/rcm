use std::{
    collections::HashMap, rc::Rc
};

use crate::{
    lexer::Token,
    analysis::Symbol,
    parser::{Expression, ExpressionId, Statement, Type}, util,
};

type BlockId = usize;
type InstId = usize;

#[derive(Copy, Clone, Debug)]
pub struct HashableFloat(pub f64);

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

macro_rules! define_instructions {
    (
        custom {
            $(
                $custom_variant:ident
                    $( ( $( $tuple_ty:ty ),* $(,)? ) )?
                    $( { $( $field:ident : $field_ty:ty ),* $(,)? } )?
            ),* $(,)?
        }
        binary {
            $( $bin_variant:ident ),* $(,)?
        }
    ) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        enum Inst {
            $(
                $custom_variant
                    $( ( $( $tuple_ty ),* ) )?
                    $( { $( $field : $field_ty ),* } )?,
            )*
            $(
                $bin_variant {
                    l: InstId,
                    r: InstId,
                },
            )*
        }
    };
}

define_instructions!{
    custom {
        Int(i64),
        Float(HashableFloat),
        Bool(bool),
        Char(char),
        String(String),

        Phi {
            operands: Vec<InstId>,
            block: BlockId,
        },
        Load(InstId),
        Store {
            addr: InstId,
            value: InstId
        },
        GEP {
            base: InstId,
            index: InstId
        },
        Address(InstId),
        Call {
            name: String,
            args: Vec<InstId>,
        },
        Param(usize), // offset
        UNDEF
    }
    binary {
        Add, Sub, Mul, Div, Mod, And, Or, Xor, LShift, RShift,
        LNot, LAnd, LOr, GT, GTE, LT, LTE, Eq, NotEq
    } 
}

#[derive(Default)]
pub enum Terminator {
    #[default]
    FallThrough,
    Return(InstId),
    Jump(BlockId),
    CondJmp(InstId, BlockId),
}

/// Kinds of basic blocks
/// Basic, Entry, Exit
#[derive(Default)]
pub enum BlockKind {
    /// Most blocks, no special properties
    #[default]
    Basic,

    // TODO: holding params to a call may be unnecessary
    /// Holds params to a call
    Entry(Vec<usize>),

    // TODO: Note to future self: I think all stack frames in my language are actually able to be
    // omitted. If not, you can add an empty Option to this field to store whether or not
    // destructuring a call stack is necessary 
    /// Exit block, this may get removed and may have no uses
    Exit,
}

pub struct BasicBlock {
    // instructions
    name: Rc<str>,
    instructions: Vec<Inst>,
    current_defs: HashMap<Rc<str>, InstId>,
    phis: Vec<InstId>,

    // CFG
    preds: Vec<BlockId>,
    succs: Vec<BlockId>,
    term: Terminator,
    kind: BlockKind,

    // construction
    incomplete: Vec<(Rc<str>, InstId)>,
    sealed: bool,
    filled: bool,
}

impl BasicBlock {
    pub fn new(name: Rc<str>) -> Self {
        Self {
            name, 
            instructions: Vec::new(),
            current_defs: HashMap::new(),
            phis: Vec::new(),

            preds: Vec::new(),
            succs: Vec::new(),
            term: Terminator::default(),
            kind: BlockKind::default(),

            incomplete: Vec::new(),
            sealed: false,
            filled: false,
        }
    }
}

struct SSABuilder {
    // old data; lookups
    exprs: Vec<Expression>,
    expr_types: HashMap<Expression, Type>,
    symbols: HashMap<Rc<str>, Symbol>,

    // output 
    blocks: Vec<BasicBlock>,
    values: Vec<Inst>,

    // lookups for SSA gen
    def_use: Vec<Vec<InstId>>,
    value_numbers: HashMap<Inst, InstId>,
    pred: BlockId,
    exit: BlockId,
    returns: Vec<InstId>,
}

impl SSABuilder {
    pub fn new(globals: HashMap<Rc<str>, Symbol>) -> Self {
        Self {
            exprs: Vec::new(),
            expr_types: HashMap::new(),
            symbols: globals,

            blocks: Vec::new(),
            values: Vec::new(),

            def_use: Vec::new(),
            value_numbers: HashMap::new(),
            pred: usize::MAX,
            exit: usize::MAX,
            returns: Vec::new(),
        }
    }

    /// adds `block` to the arena
    /// implicitly sets the `block` as the predecessor
    fn add_block(&mut self, block: BasicBlock) -> BlockId {
        let index = self.blocks.len();
        self.blocks.push(block);
        self.pred = index;
        return index;
    }

    /// adds a use to the def-use chain of `operand`
    fn add_use(&mut self, operand: InstId, user: InstId) {
        // note to self: this line existed in my original impl, but i dont see why its needed. if
        // this errors, reason through it and put it back in if needed
        debug_assert!(!self.def_use[operand].contains(&user));
        self.def_use[operand].push(user);
    }

    /// adds `value` to the arena
    /// does NOT perform value numbering
    fn add_value(&mut self, value: Inst) -> InstId {
        self.values.push(value);
        self.def_use.push(Vec::new());
        return self.values.len() - 1;
    }

    /// adds `value` to the arena
    /// does value numbering on `value`
    fn number_value(&mut self, value: Inst) -> InstId {
        match self.value_numbers.get(&value) {
            Some(&id) => id,
            None => {
                let index = self.values.len();

                // map a value to its number, allocate it
                self.value_numbers.insert(value.clone(), index);
                self.values.push(value);

                // start tracking all uses of value
                self.def_use.push(Vec::new());
                index
            }
        }
    }

    fn write_variable(&mut self, variable: Rc<str>, block: BlockId, value: InstId) {
        self.blocks[block].current_defs.insert(variable, value);
    }

    fn read_variable(&mut self, variable: Rc<str>, block: BlockId) -> InstId {
        match self.blocks[block].current_defs.get(&variable) {
            Some(value) => value.clone(),
            None => self.read_variable_recursive(variable, block),
        }
    }

    fn read_variable_recursive(&mut self, variable: Rc<str>, block: BlockId) -> InstId {
        let mut v: InstId;
        if !self.blocks[block].sealed {
            let phi = Inst::Phi{ operands: Vec::new(), block };
            v = self.number_value(phi);
            self.blocks[block].incomplete.push((variable.clone(), v));
        } else if self.blocks[block].preds.len() == 1 {
            v = self.read_variable(variable.clone(), self.blocks[block].preds[0]);
        } else {
            let phi = Inst::Phi{ operands: Vec::new(), block };
            v = self.add_value(phi);
            self.write_variable(variable.clone(), block, v);
            v = self.add_phi_operands(variable.clone(), v, block);
        }

        self.write_variable(variable, block, v);
        return v;
    }

    fn add_phi_operands(&mut self, variable: Rc<str>, phi: InstId, block: BlockId) -> InstId {
        debug_assert!(matches!(self.values[phi], Inst::Phi{ .. }));

        for pred in self.blocks[block].preds.to_owned() {
            let operand = self.read_variable(variable.clone(), pred);
            self.add_use(operand, phi);
            if let Inst::Phi{operands, .. } = &mut self.values[phi] {
                operands.push(operand);
            }
        }
 
        return self.remove_trivial_phi(phi);
    }

    // TODO: cache "witnesses" as in braun et al.
    fn remove_trivial_phi(&mut self, phi: InstId) -> InstId {
        let mut same: Option<InstId> = None;
        let (operands, block): (&Vec<InstId>, BlockId) = match &self.values[phi] {
            Inst::Phi{ operands, block} => (operands, *block),
            _ => unreachable!(),
        };

        for &op in operands {
            if Some(op) == same || op == phi { continue };
            if same.is_some() { return phi };
            same = Some(op);
        }

        let same = same.unwrap_or(self.add_value(Inst::UNDEF));
        for user in self.def_use[phi].to_owned() {
            if user == phi { continue; }
            self.reroute(user, phi, same, block);
 
            if let Inst::Phi{ .. } = self.values[user] {
                self.remove_trivial_phi(user);
            }
        }

        return same;
    }

    fn reroute(&mut self, user: InstId, old: InstId, new: InstId, block: BlockId) {
        match &mut self.values[user] {
            Inst::Add { l, r }
            | Inst::Sub { l, r }
            | Inst::Mul { l, r }
            | Inst::Div { l, r }
            | Inst::Mod { l, r }
            | Inst::And { l, r }
            | Inst::Or { l, r }
            | Inst::Xor { l, r }
            | Inst::LShift { l, r }
            | Inst::RShift { l, r }
            | Inst::LNot { l, r }
            | Inst::LAnd { l, r }
            | Inst::LOr { l, r }
            | Inst::GT { l, r }
            | Inst::GTE { l, r }
            | Inst::LT { l, r }
            | Inst::LTE { l, r }
            | Inst::Eq { l, r }
            | Inst::NotEq { l, r } => {
                if *l == old { *l = new };
                if *r == old { *r = new };
            }
            Inst::Phi{ operands, .. } => {
                for op in operands {
                    if *op == old {
                        *op = new;
                    }
                }
            }
            _ => unimplemented!(),
        }

        // remove old uses
        self.blocks[block].phis.retain(|&x| x != old);
        self.def_use[old].retain(|&x| x != user);
        self.add_use(new, user);
    }

    fn seal(&mut self, block: BlockId) {
        for (variable, phi) in std::mem::take(&mut self.blocks[block].incomplete) {
            self.add_phi_operands(variable, phi, block);
        }

        self.blocks[block].sealed = true;
    }

    fn fill(&mut self, block: BlockId) {
        self.blocks[block].filled = true;
    }

    fn cfg_edge(&mut self, pred: BlockId, succ: BlockId) {
        debug_assert!(pred != usize::MAX);
        self.blocks[pred].succs.push(succ);
        self.blocks[succ].preds.push(pred);
    }
}

impl SSABuilder {
    /// generating a basic block assumes the block has only one predecessor, and marks it as sealed
    /// accordingly. Otherwise, generate blocks with `BasicBlock::new(name: Rc<str>);`
    fn statement(&mut self, stmt: Statement) {
        match stmt {
            Statement::ParseError => unreachable!(),
            Statement::FunctionDeclaration {
                name,
                return_type,
                parameters,
                body,
                ..
            } => {
                let mut entry_block = BasicBlock::new(Rc::from("function entry"));
                let entry = self.add_block(entry_block);
                self.seal(entry);

                // TODO: may be unnecssary, refer to BlockKind enum
                let mut params = Vec::with_capacity(parameters.len());
                let mut total_offset = 0;
                for p in parameters {
                    let Statement::Parameter { name, t } = *p else { unreachable!() };

                    params.push(total_offset);
                    let param = Inst::Param(total_offset);
                    total_offset += util::get_size(&t);

                    let param_id = self.add_value(param);
                    self.write_variable(Rc::from(name), entry, param_id);
                }

                let mut exit_block = BasicBlock::new(Rc::from("function entry"));
                self.exit = self.add_block(exit_block);

                self.fill(entry);
                self.statement(*body);

                // TODO: add returns to a single instruction

                self.cfg_edge(self.pred, self.exit);
            }
            // handled in function declaration
            Statement::Parameter{..} => unreachable!(),
            Statement::Block(stmts) => {
                let b = self.add_block(BasicBlock::new(Rc::from("Basic Block")));
                self.cfg_edge(self.pred, b);
                self.seal(b);

                for s in stmts {
                    self.statement(*s);
                }

                self.fill(b);
            } 
            Statement::IfStatement { condition, block, alt } => {
                let entry = self.add_block(BasicBlock::new(Rc::from("if condition")));
                self.seal(entry);

                // the conditions hold no instructions, just a conditional jump as a terminating value
                self.fill(entry);
            }
            Statement::WhileStatement { condition, block } => {
                // TODO: this needs a lot of thought on how i want to do conditionals to optimize
                // for fallthrough and short circuiting
            }
            Statement::StructDeclaration { 
                name, 
                members, 
                methods, 
                public 
            } => {
                // TODO: not completely sure why this doesnt touch members
                for method in methods {
                    self.statement(*method);
                }
            }
            Statement::Member { .. } => return,
            Statement::VariableDeclaration { 
                identifier, 
                variable_type, 
                initial_value, 
                constant, 
                .. 
            } => {
                if let Some(e) = initial_value {
                    let rhs = self.expr(e);
                    self.write_variable(Rc::from(identifier), self.pred, rhs);
                } else {
                    let val = self.add_value(Inst::UNDEF);
                    self.write_variable(Rc::from(identifier), self.pred, val);
                }
            }
            Statement::Return { value } => {
                if let Some(expr) = value {
                    // TODO: temporarily set a flag to inline instructions, so something like `call`
                    // doesnt emit an extra instruction
                    // either that or figure out the best way to generate temporary variables when
                    // they are necessary
                    let ret = self.expr(expr);
                    self.returns.push(ret);
                }

                self.cfg_edge(self.pred, self.exit);
            }
            Statement::ExpressionStatement(expr) => self.expr(expr),
        }
    }
}

// // conditional logic
// impl SSABuilder {
//     /// turns all logical ands into ors with de morgans laws, which makes the conditions far easier
//     /// to work with. Returns a vector of conditions, each connceted by a logical or
//     fn flatten_conditional(&self, e: ExpressionId) -> Vec<Inst> {
//         let mut out: Vec<Inst> = Vec::new();
//         self.nested_conditional(e, &mut out);
//         return out;
//     }
//
//     #[inline]
//     fn nested_conditional(&self, e: ExpressionId, out: &mut Vec<Inst>) {
//         match self.exprs[e] {
//             Expression::Bool(b) => out.push(Inst::Bool(b)),
//             Expression::Unary { operator, member } => {
//                 if !matches!(operator, Token::Bang) { panic!(); }
//                 out.push(self.invert_conditional(member));
//             }
//             Expression::Binary { lhs, operator, rhs } => {
//                 match operator {
//                     Token::DoubleAmpersand => {
//                         out.push(self.invert_conditional(lhs));
//                         out.push(self.invert_conditional(rhs));
//                     }
//                     Token::DoublePipe => {
//                         self.nested_conditional(lhs, out);
//                         self.nested_conditional(rhs, out);
//                     }
//
//                     Token::EqualEqual => out.push(Inst::Eq { l: lhs, r: rhs }),
//
//                     Token::EqualEqual => self.exprs[expr] = Expression::Binary{ lhs: *lhs, operator: Token::BangEqual, rhs: *rhs},
//                     Token::BangEqual => self.exprs[expr] = Expression::Binary{ lhs: *lhs, operator: Token::EqualEqual, rhs: *rhs},
//                     Token::LeftCaret => {
//                         self.invert_condition(*lhs);
//                         self.invert_condition(*rhs);
//                         self.exprs[expr] = Expression::Binary{
//                             lhs: *lhs, operator: Token::RightCaretEqual, rhs: *rhs
//                         }
//                     }
//                     Token::RightCaret => {
//                         self.invert_condition(*lhs);
//                         self.invert_condition(*rhs);
//                         self.exprs[expr] = Expression::Binary{
//                             lhs: *lhs, operator: Token::LeftCaretEqual, rhs: *rhs
//                         }
//                     }
//                     Token::LeftCaretEqual => {
//                         self.invert_condition(*lhs);
//                         self.invert_condition(*rhs);
//                         self.exprs[expr] = Expression::Binary{
//                             lhs: *lhs, operator: Token::RightCaret, rhs: *rhs
//                         }
//                     }
//                     Token::RightCaretEqual => {
//                         self.invert_condition(*lhs);
//                         self.invert_condition(*rhs);
//                         self.exprs[expr] = Expression::Binary{
//                             lhs: *lhs, operator: Token::LeftCaret, rhs: *rhs
//                         }
//                     }
//                 }
//             }
//         }
//     }
//
//     /// Inverts a conditional
//     fn invert_conditional(&self, e: ExpressionId) -> Inst {
//     }
// }

// // i am not even a little bit proud of this code. fixing it means rewriting parser with Box again,
// // and i just dont want to do that right now...
// // TODO: rewrite parser with Box<T> or Rc<RefCell<T>>
// impl SSABuilder {
//     /// mutates the value at the arena in place to invert a logical condition
//     /// recurses for all children
//     fn invert_condition(&mut self, expr: ExpressionId) {
//         match &self.exprs[expr].clone() {
//             Expression::Bool(b) => self.exprs[expr] = Expression::Bool(!b),
//             Expression::Unary{ operator, member } => {
//                 if !matches!(operator, Token::Bang) { panic!() };
//                 self.exprs[expr] = self.exprs[*member].clone();
//             }
//             Expression::Binary { lhs, operator, rhs } => {
//                 match operator {
//                     Token::EqualEqual => self.exprs[expr] = Expression::Binary{ lhs: *lhs, operator: Token::BangEqual, rhs: *rhs},
//                     Token::BangEqual => self.exprs[expr] = Expression::Binary{ lhs: *lhs, operator: Token::EqualEqual, rhs: *rhs},
//                     Token::DoubleAmpersand => {
//                         self.invert_condition(*lhs);
//                         self.invert_condition(*rhs);
//                         self.exprs[expr] = Expression::Binary{
//                             lhs: *lhs, operator: Token::DoublePipe, rhs: *rhs
//                         }
//                     }
//                     Token::DoublePipe => {
//                         self.invert_condition(*lhs);
//                         self.invert_condition(*rhs);
//                         self.exprs[expr] = Expression::Binary{
//                             lhs: *lhs, operator: Token::DoubleAmpersand, rhs: *rhs
//                         }
//                     }
//                     Token::LeftCaret => {
//                         self.invert_condition(*lhs);
//                         self.invert_condition(*rhs);
//                         self.exprs[expr] = Expression::Binary{
//                             lhs: *lhs, operator: Token::RightCaretEqual, rhs: *rhs
//                         }
//                     }
//                     Token::RightCaret => {
//                         self.invert_condition(*lhs);
//                         self.invert_condition(*rhs);
//                         self.exprs[expr] = Expression::Binary{
//                             lhs: *lhs, operator: Token::LeftCaretEqual, rhs: *rhs
//                         }
//                     }
//                     Token::LeftCaretEqual => {
//                         self.invert_condition(*lhs);
//                         self.invert_condition(*rhs);
//                         self.exprs[expr] = Expression::Binary{
//                             lhs: *lhs, operator: Token::RightCaret, rhs: *rhs
//                         }
//                     }
//                     Token::RightCaretEqual => {
//                         self.invert_condition(*lhs);
//                         self.invert_condition(*rhs);
//                         self.exprs[expr] = Expression::Binary{
//                             lhs: *lhs, operator: Token::LeftCaret, rhs: *rhs
//                         }
//                     }
//                     _ => panic!(),
//                 }
//             }
//             _ => panic!(),
//         }
//     }
// }
