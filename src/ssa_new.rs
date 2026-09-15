use std::collections::HashMap;
use crate::{
    analysis::Symbol,
    parser::{Expression, Type}
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
        #[derive(Debug, Clone, PartialEq, Eq)]
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

        Phi(Vec<InstId>),
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

    sealed: bool,
    filled: bool
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

            sealed: false,
            filled: false
        }
    }
}

struct SSAGenerator {
    exprs: Vec<Expression>,
    expr_types: HashMap<Expression, Type>,

    blocks: Vec<BasicBlock>,
    symbols: HashMap<String, Symbol>,
    // and many more...
}

