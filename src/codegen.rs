use crate::parser::{Type};
use crate::ssa::{IR, BlockId};
use crate::ssa;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug)]
pub enum Value {
    Byte(u8),
    Word(u16),
    DWord(u32),
    QWord(u64),
    Usize(usize),
}

#[derive(Copy, Clone, Debug)]
pub struct GPR {
    pub num: usize,
    pub size: usize,
}

#[derive(Copy, Clone, Debug)]
pub enum Register {
    GPR(GPR),
    RBP,
    RSP,
}

#[derive(Copy, Clone, Debug)]
pub enum Location {
    StackOffset(usize),
    Register(Register),
    Inline(Value)
}

#[derive(Debug)]
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

#[derive(Debug)]
struct BasicBlock {
    // label: String,
    block_id: BlockId, 
    instructions: Vec<Asm>
}

impl BasicBlock {
    pub fn new(id: BlockId) -> Self {
        Self {
            block_id: id,
            instructions: Vec::new(),
        }
    }

    pub fn add(&mut self, inst: Asm) {
        self.instructions.push(inst);
    }
}

pub struct Codegen<'a> {
    ir: &'a IR,
    locations: HashMap<ssa::ValueId, Location>,
    blocks: Vec<BasicBlock>
    // offset: usize,
}

impl<'a> Codegen<'a> {
    pub fn new(ir: &'a IR) -> Self {
        Self {
            ir,
            locations: HashMap::new(),
            blocks: Vec::new(),
            // offset: 0,
        }
    }

    fn emit_block(&mut self, block: BasicBlock) {
        self.blocks.push(block);
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

    fn get_size(&self, value: &ssa::ValueId) -> usize {
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

    fn assign_locations(&mut self, value: &ssa::ValueId) -> Vec<(ssa::ValueId, Location)> {
        let mut stack: Vec<(ssa::ValueId, Location)> = Vec::new();

        match &self.ir.values[*value].kind {
            ssa::ValueKind::Add { lhs, rhs }
            | ssa::ValueKind::Sub { lhs, rhs }
            | ssa::ValueKind::Mul { lhs, rhs }
            | ssa::ValueKind::Div { lhs, rhs }
            | ssa::ValueKind::Mod { lhs, rhs } => {
                stack.extend(self.assign_locations(lhs));
                stack.extend(self.assign_locations(rhs));
                let offset = Location::StackOffset(self.get_size(value));
                stack.push((*value, offset));
            }
            ssa::ValueKind::Array { elements } => todo!(),
            ssa::ValueKind::Struct { identifier, members } => {
                todo!();
            }
            ssa::ValueKind::Ret { value } => {
                stack.extend(self.assign_locations(value));
            },
            ssa::ValueKind::Store { .. } => (),
            _ => (), // primatives are inlined
        }

        return stack;
    }

    // maps ValueId to Offset per BlockId
    fn preallocate(&mut self, block: BlockId) -> usize {
        let mut total: usize = 0;
        for inst in &self.ir.blocks[block].instructions {
            for (value, location) in self.assign_locations(inst) {
                if let Location::StackOffset(offset) = location {
                    total += offset;
                }

                self.locations.insert(value, location);
            }
        }

        return total;
    }

    fn get_location(&mut self, value: &ssa::ValueId) -> &Location {
        self.locations.get(value).unwrap()
    }

    pub fn create_asm(&mut self, entry: BlockId) {
        let mut block = BasicBlock::new(entry);
        let offset = self.preallocate(entry);
        if offset > 0 {
            let inst = Asm::Sub(
                Location::Register(Register::RSP),
                Location::Inline(Value::Usize(offset))
            );

            block.add(inst);
        }

        dbg!(block);

        for inst in &self.ir.blocks[entry].instructions {
            match &self.ir.values[*inst].kind {
                ssa::ValueKind::Add { lhs, rhs } => {
                    let olhs = self.get_location(lhs);
                    let orhs = self.get_location(rhs);
                }
                ssa::ValueKind::Ret { value } => {
                    let val = self.get_location(value);
                }
                ssa::ValueKind::Jump(block_id) => {
                    // TODO: i feel like i should give this more thought and make sure a block id is
                    // actually what i want
                    dbg!(Asm::Jmp(*block_id));
                }
                n @ _ => {
                    dbg!(n);
                    unimplemented!();
                }
            }
        }

        // dfs children
        for block in &self.ir.blocks[entry].successors {
            self.create_asm(*block);
        }
    }
}

