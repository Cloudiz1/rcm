use crate::parser::{Type};
use crate::ssa::{IR, BlockId, Value, ValueId};
use std::collections::HashMap;

// will eventually contain registers and such
#[derive(Copy, Clone)]
enum Location {
    StackOffset(usize),
}

enum MachineInstructions {
    Mov(Location, Location),
    Add(Location, Location),
    Sub(Location, Location),
    Mul(Location, Location),
    Div(Location, Location),
    Mod(Location, Location),
}

struct Codegen {
    ir: IR,
    locations: HashMap<ValueId, Location>,
    offset: usize,
}

impl Codegen {
    pub fn new(ir: IR) -> Self {
        Self {
            ir,
            locations: HashMap::new(),
            offset: 0,
        }
    }

    fn get_location(&mut self, value: ValueId) -> Location {
        if let Some(loc) = self.locations.get(&value) { return *loc }
        else {
            self.offset += self.get_offset(value);
            let loc = Location::StackOffset(self.offset);
            self.locations.insert(value, loc);
            return loc;
        }
    }

    fn get_offset(&self, value: ValueId) -> usize {
        match self.ir.values[value].t {
            Type::Str |
            Type::Pointer(_) |
            Type::Array{ .. } |
            Type::Usize => std::mem::size_of::<usize>(),
            Type::I8 => 1,
            Type::U8 => 1,
            Type::I16 => 2,
            Type::U16 => 2,
            Type::I32 => 4,
            Type::U32 => 4,
            Type::I64 => 8,
            Type::U64 => 8,
            Type::F16 => 2,
            Type::F32 => 4,
            Type::F64 => 8,
            Type::Char => 1,
            Type::Bool => 1,
            Type::Void => 0,
            Type::Unknown => 0,
            _ => panic!("fuck"),
        }
    } 
}

pub fn codegen(ir: IR, block: BlockId) {
    for inst in &ir.blocks[block].instructions {
        match ir.values[*inst] {
            Value::Add { lhs, rhs } => {

            }
            _ => unimplemented!(),
        }
    }
}
