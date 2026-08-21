use crate::parser::{Type};
use crate::ssa::{IR, BlockId};
use crate::ssa;
use std::collections::HashMap;
use std::cmp::max;

#[derive(Copy, Clone, Debug)]
pub enum GPR {
    AX,
    BX,
    CX,
    DX,
    SI,
    DI,
    R8,
    R9, 
    R10, 
    R11,
    R12,
    R13,
    R14,
    R15,
}

#[derive(Copy, Clone, Debug)]
pub enum Register {
    GPR {
        kind: GPR,
        size: usize,
    },
    RSP, // yes, these ARE GPRs, but I think the distinction is good
    RBP
}

#[derive(Copy, Clone, Debug)]
enum Primative {
    Int(i64),
    Float(f64),
    // TODO: many more...
} 


#[derive(Copy, Clone, Debug)]
struct Value {
    size: usize,
    prim: Primative
}

impl Value {
    pub fn new(size: usize, prim: Primative) -> Self {
        Self {
            size,
            prim
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Location {
    StackOffset(usize),
    Register(Register),
    Inline(Primative),
}

#[derive(Debug, Copy, Clone)]
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
    Ret
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

    pub fn push_inst(&mut self, inst: Asm) {
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

    fn ssa_val_to_prim(&self, value: ssa::ValueId) -> Primative {
        match self.ir.values[value].kind {
            ssa::ValueKind::Int(val) => Primative::Int(val),
            ssa::ValueKind::Float(val) => Primative::Float(val.0),
            _ => panic!("not primative")
        }
    }

    fn get_size(&self, value: ssa::ValueId) -> usize {
        match self.ir.values[value].t {
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

    fn assign_locations(&mut self, value: ssa::ValueId) -> Vec<(ssa::ValueId, Location)> {
        let mut stack: Vec<(ssa::ValueId, Location)> = Vec::new();

        match &self.ir.values[value].kind {
            &ssa::ValueKind::Add { lhs, rhs }
            | &ssa::ValueKind::Sub { lhs, rhs }
            | &ssa::ValueKind::Mul { lhs, rhs }
            | &ssa::ValueKind::Div { lhs, rhs }
            | &ssa::ValueKind::Mod { lhs, rhs } => {
                // TODO: id assume this is cut post ralloc
                stack.extend(self.assign_locations(lhs));
                stack.extend(self.assign_locations(rhs));
                let offset = Location::StackOffset(self.get_size(value));
                stack.push((value, offset));
            }
            ssa::ValueKind::Array { elements } => todo!(),
            ssa::ValueKind::Struct { identifier, members } => {
                todo!();
            }
            &ssa::ValueKind::Ret { value } => {
                stack.extend(self.assign_locations(value));
            },
            ssa::ValueKind::Store { .. } => (),
            _ => (), // primatives are inlined
        }

        return stack;
    }

    // maps ValueId to Offset per BlockId
    fn block_size(&mut self, block: BlockId) -> usize {
        let mut total: usize = 0;
        for &inst in &self.ir.blocks[block].instructions {
            for (value, location) in self.assign_locations(inst) {
                if let Location::StackOffset(offset) = location {
                    total += offset;
                }

                self.locations.insert(value, location);
            }
        }

        return total;
    }

    fn get_location(&self, value: ssa::ValueId) -> Location {
        // if matches!(self.ir.values[value].kind, ssa::ValueKind::Call { .. }) {
        //     // TODO: functions
        //     let loc = Location::Register(Register::GPR {
        //         r: GPR::AX,
        //         size: self.get_size(value),
        //     });
        // }

        match self.locations.get(&value) {
            Some(&v) => v,
            None => {
                let prim = self.ssa_val_to_prim(value);
                Location::Inline(prim)
            },
        }
    }

    pub fn create_block(&mut self, entry: BlockId) {
        let mut block = BasicBlock::new(entry);
        let size = self.block_size(entry);
        if size > 0 {
            // TODO: this only needs to be set up if:
            // basic block contains a function call
            // uses >128 bytes (red zone i think)
            // has a load instruction (needs to copy it into stack)
            let prim = Primative::Int(size as i64);
            let inst = Asm::Sub(
                Location::Register(Register::RSP),
                Location::Inline(prim)
            );

            block.push_inst(inst);
        }

        for &inst in &self.ir.blocks[entry].instructions {
            self.create_asm(&mut block, inst);
        }

        // dfs children
        for block in &self.ir.blocks[entry].successors {
            self.create_block(*block);
        }

        dbg!(block);
    }

    fn is_primitive(&self, value: usize) -> bool {
        match &self.ir.values[value].kind {
            ssa::ValueKind::Int(_) 
            | ssa::ValueKind::Float(_) 
            | ssa::ValueKind::Bool(_) 
            // | ssa::ValueKind::String(_) 
            | ssa::ValueKind::Char(_) => true,
            _ => false
        }
    }

    fn gen_deps(&self, block: &mut BasicBlock, value: ssa::ValueId) {
        if !self.is_primitive(value) {
            self.create_asm(block, value);
        }
    }

    fn create_asm(&self, block: &mut BasicBlock, value: ssa::ValueId) {
        match &self.ir.values[value].kind {
            &ssa::ValueKind::Add { lhs, rhs } => {
                self.gen_deps(block, lhs);
                self.gen_deps(block, rhs);

                let out_lhs = self.get_location(lhs);
                let out_rhs = self.get_location(rhs);
                let size = self.get_size(value);

                // if either lives in a register, just reuse it
                if matches!(out_rhs, Location::Register(_)) {
                    block.push_inst(Asm::Add(out_rhs, out_lhs));
                    return
                } 
                else if matches!(out_lhs, Location::Register(_)) {
                    block.push_inst(Asm::Add(out_lhs, out_rhs));
                    return
                }

                // otherwise, default to rax
                let reg = Location::Register(Register::GPR {
                    kind: GPR::AX,
                    size
                });

                block.push_inst(Asm::Mov(reg, out_lhs));
                block.push_inst(Asm::Add(reg, out_rhs));
            }
            &ssa::ValueKind::Ret { value } => {
                self.gen_deps(block, value);

                let reg = Location::Register(Register::GPR { 
                    kind: GPR::AX,
                    size: self.get_size(value) 
                });

                block.push_inst(Asm::Mov(reg, self.get_location(value)));
                block.push_inst(Asm::Ret);
            }
            ssa::ValueKind::Jump(block_id) => {
                // TODO: i feel like i should give this more thought and make sure a block id is
                // actually what i want
                Asm::Jmp(*block_id);
            }
            n @ _ => {
                dbg!(n);
                unimplemented!();
            }
        }
    }

    // pub fn create_asm(&mut self, entry: BlockId) {
    //     let mut block = BasicBlock::new(entry);
    //     let offset = self.preallocate(entry);
    //     if offset > 0 {
    //         let inst = Asm::Sub(
    //             Location::Register(Register::RSP),
    //             Location::Inline(Value::QWord(offset as u64))
    //         );
    //
    //         block.add(inst);
    //     }
    //
    //     for inst in &self.ir.blocks[entry].instructions {
    //         match &self.ir.values[*inst].kind {
    //             ssa::ValueKind::Add { lhs, rhs } => {
    //                 let olhs = self.get_location(lhs);
    //                 let orhs = self.get_location(rhs);
    //
    //                 let size_lhs = self.get_size(lhs);
    //                 let size_rhs = self.get_size(rhs);
    //                 let size = max(size_lhs, size_rhs);
    //
    //                 if matches!(olhs, Location::StackOffset(_))
    //                 || matches!(orhs, Location::StackOffset(_)) {
    //                     let reg = Location::Register(Register::GPR {
    //                         r: GPR::AX,
    //                         size
    //                     });
    //
    //                     block.add(Asm::Mov(reg, *olhs));
    //                     block.add(Asm::Add(reg, *orhs));
    //                 }
    //             }
    //             ssa::ValueKind::Ret { value } => {
    //                 dbg!(&self.ir.values[*value]);
    //                 let reg = Location::Register(Register::GPR { 
    //                     r: GPR::AX,
    //                     size: self.get_size(value) 
    //                 });
    //
    //                 block.add(Asm::Mov(reg, *self.get_location(value)));
    //                 block.add(Asm::Ret);
    //             }
    //             ssa::ValueKind::Jump(block_id) => {
    //                 // TODO: i feel like i should give this more thought and make sure a block id is
    //                 // actually what i want
    //                 Asm::Jmp(*block_id);
    //             }
    //             n @ _ => {
    //                 dbg!(n);
    //                 unimplemented!();
    //             }
    //         }
    //     }
    //
    //     // dfs children
    //     for block in &self.ir.blocks[entry].successors {
    //         self.create_asm(*block);
    //     }
    //
    //     dbg!(block);
    // }
}

