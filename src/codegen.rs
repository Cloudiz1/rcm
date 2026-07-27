use crate::parser::{Type};
use crate::ssa::{IR, BlockId, Value, ValueKind, ValueId};
use std::collections::HashMap;

// will eventually contain registers and such
#[derive(Copy, Clone)]
pub enum Location {
    StackOffset(usize),
}

pub enum Asm {
    Mov(Location, Location),
    Add(Location, Location),
    Sub(Location, Location),
    Mul(Location, Location),
    IMul(Location, Location),
    Div(Location, Location),
    IDiv(Location, Location),
    Mod(Location, Location),
    Cmp(Location, Location),
    Label(BlockId),
    Jmp(BlockId),
    Je(BlockId),
}

pub struct Codegen<'a> {
    ir: &'a IR,
    locations: HashMap<ValueId, Location>,
    offset: usize,
}

impl<'a> Codegen<'a> {
    pub fn new(ir: &'a IR) -> Self {
        Self {
            ir,
            locations: HashMap::new(),
            offset: 0,
        }
    }

    // fn get_location(&mut self, value: ValueId) -> Location {
    //     if let Some(loc) = self.locations.get(&value) { return *loc }
    //     else {
    //         self.offset += self.get_size(&value);
    //         let loc = Location::StackOffset(self.offset);
    //         self.locations.insert(value, loc);
    //         return loc;
    //     }
    // }

    fn get_size(&self, value: &ValueId) -> usize {
        match self.ir.values[*value].t {
            Type::Str
            | Type::Pointer(_)
            | Type::Array{ .. }
            | Type::Usize => std::mem::size_of::<usize>(),
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

    fn assign_locations(&mut self, value: &ValueId) -> Vec<(ValueId, Location)> {
        let mut stack: Vec<(ValueId, Location)> = Vec::new();

        match &self.ir.values[*value].kind {
            ValueKind::Add { lhs, rhs }
            | ValueKind::Sub { lhs, rhs }
            | ValueKind::Mul { lhs, rhs }
            | ValueKind::Div { lhs, rhs }
            | ValueKind::Mod { lhs, rhs } => {
                self.assign_locations(lhs);
                self.assign_locations(rhs);
                let offset = Location::StackOffset(self.get_size(value));
                stack.push((*value, offset));
            }
            ValueKind::Array { elements } => todo!(),
            ValueKind::Struct { identifier, members } => {
                todo!();
            }
            ValueKind::Ret { .. }
            | ValueKind::Store { .. } => (),
            _ => {
                let offset = Location::StackOffset(self.get_size(value));
                stack.push((*value, offset));
            }
        }

        return stack;
    }

    // maps ValueId to Offset per BlockId
    fn preallocate(&mut self, block: BlockId) -> (HashMap<ValueId, Location>, usize) {
        let mut allocations: HashMap<ValueId, Location> = HashMap::new();
        let mut total: usize = 0;
        for inst in &self.ir.blocks[block].instructions {
            for (value, location) in self.assign_locations(inst) {
                if let Location::StackOffset(offset) = location {
                    total += offset;
                }

                allocations.insert(value, location);
            }
        }

        return (allocations, total);
    }

    pub fn create_asm(&mut self, block: BlockId) {
        let (allocations, total_offset) = self.preallocate(block);
        println!("sub rbp, {}", total_offset);

        // for inst in &ir.blocks[block].instructions {
        //     match ir.values[*inst].kind {
        //         ValueKind::Add { lhs, rhs } => {
        //             let olhs = self.get_location(lhs);
        //             let orhs = self.get_location(rhs);
        //         }
        //         ValueKind::Sub { lhs, rhs } => {
        //             let olhs = self.get_location(lhs);
        //             let orhs = self.get_location(rhs);
        //         }
        //         ValueKind::Mul { lhs, rhs } => {
        //             let olhs = self.get_location(lhs);
        //             let orhs = self.get_location(rhs);
        //         }
        //         _ => unimplemented!(),
        //     }
        // }
    }
}

