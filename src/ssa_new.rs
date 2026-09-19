use std::collections::HashMap;
use crate::{
    analysis::Symbol,
    parser::{Expression, Type}, ssa::ValueKind
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

        CondJmp(InstId, BlockId),

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
        UNDEF
    }
    binary {
        Add, Sub, Mul, Div, Mod, And, Or, Xor, LShift, RShift,
        LNot, LAnd, LOr, GT, GTE, LT, LTE, Eq, NotEq
    }
}

#[derive(Default)]
enum Terminator {
    #[default]
    FallThrough,
Return(InstId),
    Jump(BlockId)
}

pub struct BasicBlock {
    name: String,
    instructions: Vec<Inst>,
    current_defs: HashMap<String, InstId>,
    phis: Vec<InstId>,

    preds: Vec<BlockId>,
    succs: Vec<BlockId>,
    term: Terminator,

    incomplete: Vec<(String, InstId)>,
    sealed: bool,
    filled: bool,
}

impl BasicBlock {
    pub fn new(name: String) -> Self {
        Self {
            name, 
            instructions: Vec::new(),
            current_defs: HashMap::new(),
            phis: Vec::new(),

            preds: Vec::new(),
            succs: Vec::new(),
            term: Terminator::default(),

            incomplete: Vec::new(),
            sealed: false,
            filled: false
        }
    }
}

struct SSABuilder {
    exprs: Vec<Expression>,
    expr_types: HashMap<Expression, Type>,
    symbols: HashMap<String, Symbol>,

    blocks: Vec<BasicBlock>,
    def_use: Vec<Vec<InstId>>,
    values: Vec<Inst>,
    value_numbers: HashMap<Inst, InstId>,
}

impl SSABuilder {
    pub fn new(globals: HashMap<String, Symbol>) -> Self {
        Self {
            exprs: Vec::new(),
            expr_types: HashMap::new(),
            symbols: globals,

            blocks: Vec::new(),
            def_use: Vec::new(),
            values: Vec::new(),
            value_numbers: HashMap::new(),
        }
    }

    fn add_use(&mut self, operand: InstId, user: InstId) {
        debug_assert!(!self.def_use[operand].contains(&user));
        self.def_use[operand].push(user);
    }

    fn add_value(&mut self, value: Inst) -> InstId {
        self.values.push(value);
        self.def_use.push(Vec::new());
        return self.values.len() - 1;
    }

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

    fn write_variable(&mut self, variable: String, block: BlockId, value: InstId) {
        self.blocks[block].current_defs.insert(variable, value);
    }

    fn read_variable(&mut self, variable: &String, block: BlockId) -> InstId {
        match self.blocks[block].current_defs.get(variable) {
            Some(value) => value.clone(),
            None => self.read_variable_recursive(variable, block),
        }
    }

    fn read_variable_recursive(&mut self, variable: &String, block: BlockId) -> InstId {
        let mut v: InstId;
        if !self.blocks[block].sealed {
            let phi = Inst::Phi{ operands: Vec::new(), block };
            v = self.number_value(phi);
            self.blocks[block].incomplete.push((variable.clone(), v));
        } else if self.blocks[block].preds.len() == 1 {
            v = self.read_variable(variable, self.blocks[block].preds[0]);
        } else {
            let phi = Inst::Phi{ operands: Vec::new(), block };
            v = self.add_value(phi);
            self.write_variable(variable.clone(), block, v);
            v = self.add_phi_operands(variable, v, block);
        }

        v
    }

    fn add_phi_operands(&mut self, variable: &String, phi: InstId, block: BlockId) -> InstId {
        debug_assert!(matches!(self.values[phi], Inst::Phi{ .. }));

        for pred in self.blocks[block].preds.to_owned() {
            let operand = self.read_variable(variable, pred);
            self.add_use(operand, phi);
            if let Inst::Phi{operands, .. } = &mut self.values[phi] {
                operands.push(operand);
            }
        }
 
        return self.remove_trivial_phi(phi);
    }

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

        self.blocks[block].phis.retain(|&x| x != old);
        self.def_use[old].retain(|&x| x != user);
        self.add_use(new, user);
    }

    fn seal_block(&mut self, block: BlockId) {
        for (variable, phi) in std::mem::take(&mut self.blocks[block].incomplete) {
            self.add_phi_operands(&variable, phi, block);
        }

        self.blocks[block].sealed = true;
    }
}
