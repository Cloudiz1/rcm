use crate::util;
use crate::ssa::{IR, BlockId};
use crate::ssa; // as to not pollute with ssa::{ Value, ValueKind, ValueId }
use crate::analysis::Symbol;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug)]
pub enum GPR {
    A,
    B,
    C,
    D,
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

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::RSP => write!(f, "RSP"),
            Register::RBP => write!(f, "RBP"),
            Register::GPR { kind, size } => {
                let name = match kind {
                    GPR::A => "A",
                    GPR::B => "B",
                    GPR::C => "C",
                    GPR::D => "D",
                    _ => {
                        dbg!(kind);
                        unimplemented!();
                    }
                };
                match size {
                    1 => write!(f, "{name}L"),
                    2 => write!(f, "{name}X"),
                    4 => write!(f, "E{name}X"),
                    8 => write!(f, "R{name}X"),
                    _ => panic!("invalid register size"),
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Immediate {
    Int(i64),
    Float(f64),
    // TODO: many more...
} 

impl std::fmt::Display for Immediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Immediate::Int(val) => write!(f, "{}", val),
            Immediate::Float(val) => write!(f, "{}", val),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Location {
    ParamOffset(usize),
    StackOffset(usize),
    Register(Register),
    Immediate(Immediate),
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Location::ParamOffset(size) => write!(f, "[rbp+{}]", size),
            Location::StackOffset(size) => write!(f, "[rbp-{}]", size),
            Location::Register(reg) => reg.fmt(f),
            Location::Immediate(imm) => imm.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
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
    Call(String),
    Push(Location),
    Pop(Location),
    Ret
}

#[derive(Debug)]
pub struct BasicBlock {
    pub label: String,
    pub id: BlockId, 
    pub instructions: Vec<Asm>,
    pub kind: ssa::BlockKind,
}

impl BasicBlock {
    pub fn new(id: BlockId, label: String, kind: ssa::BlockKind) -> Self {
        Self {
            label,
            id,
            instructions: Vec::new(),
            kind,
        }
    }

    pub fn push_inst(&mut self, inst: Asm) {
        self.instructions.push(inst);
    }
}

pub struct Codegen<'a> {
    ir: &'a IR,
    locations: HashMap<ssa::ValueId, Location>,
    blocks: Vec<BasicBlock>,
    offset: usize,
}

impl<'a> Codegen<'a> {
    pub fn new(ir: &'a IR) -> Self {
        Self {
            ir,
            locations: HashMap::new(),
            blocks: Vec::new(),
            offset: 0,
        }
    }

    fn emit_block(&mut self, block: BasicBlock) {
        self.blocks.push(block);
    }

    fn ssa_val_to_prim(&self, value: ssa::ValueId) -> Immediate {
        match self.ir.values[value].kind {
            ssa::ValueKind::Int(val) => Immediate::Int(val),
            ssa::ValueKind::Float(val) => Immediate::Float(val.0),
            _ => panic!("not primative")
        }
    }

    fn get_size(&self, value: ssa::ValueId) -> usize {
        util::get_size(&self.ir.values[value].t)
    }

    fn instruction_size(&self, value: ssa::ValueId) -> usize {
        match &self.ir.values[value].kind {
            &ssa::ValueKind::Add { lhs, rhs }
            | &ssa::ValueKind::Sub { lhs, rhs }
            | &ssa::ValueKind::Mul { lhs, rhs }
            | &ssa::ValueKind::Div { lhs, rhs }
            | &ssa::ValueKind::Mod { lhs, rhs } => {
                return self.get_size(value) + self.instruction_size(lhs) + self.instruction_size(rhs);
            }
            ssa::ValueKind::Array { elements } => todo!(),
            ssa::ValueKind::Struct { identifier, members } => {
                todo!();
            }
            &ssa::ValueKind::Ret { value } => {
                return self.instruction_size(value);
            },
            ssa::ValueKind::Store { .. } => 0,
            _ => 0, // primatives are inlined
        }
    }

    fn block_size(&mut self, block: BlockId) -> usize {
        let mut total: usize = 0;
        for &inst in &self.ir.blocks[block].instructions {
            total += self.instruction_size(inst);
        }

        return total;
    }

    fn get_location(&self, value: ssa::ValueId) -> Location {
        if let Some(&v) = self.locations.get(&value) {
            return v;
        }

        // value can still be a plethera of things, namely:
        //  - Immediate
        //  - function call
        //  - parameter
        match &self.ir.values[value].kind {
            ssa::ValueKind::Call { name, .. } => { 
                // for now, all return values live in RAX
                let Symbol::Function { return_type, .. } = self.ir.symbols.get(name)
                    .expect("Codegen::get_location, expected function, is None") else {
                    panic!("Codegen::get_location, expected function");
                };

                Location::Register(Register::GPR { 
                    kind: GPR::A, 
                    size: util::get_size(return_type)
                })
            }
            ssa::ValueKind::Param { offset } => {
                const PARAM_OFFSET: usize = 16; // rbp + rsp
                Location::ParamOffset(PARAM_OFFSET + offset)
            }
            _ => {
                // primateive, hopefully
                let prim = self.ssa_val_to_prim(value);
                Location::Immediate(prim)
            }
        }
    }

    pub fn create_block(&mut self, entry: BlockId) {
        // TODO: needs a bit more than that...
        let mut block = BasicBlock::new(
            entry,
            self.ir.blocks[entry].name.clone(),
            self.ir.blocks[entry].kind
        );

        let size = self.block_size(entry);

        if matches!(self.ir.blocks[entry].kind, ssa::BlockKind::FunctionEntry) {
            let rbp = Asm::Push(Location::Register(Register::RBP));
            let rsp = Asm::Mov(
                Location::Register(Register::RBP),
                Location::Register(Register::RSP)
            );

            block.push_inst(rbp);
            block.push_inst(rsp);
        }

        if size > 0 {
            // TODO: this only needs to be set up if:
            // basic block contains a function call
            // uses >128 bytes (red zone i think)
            // has a load instruction (needs to copy it into stack)
            let prim = Immediate::Int(size as i64);
            let inst = Asm::Sub(
                Location::Register(Register::RSP),
                Location::Immediate(prim)
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

        self.emit_block(block);
    }

    fn stack_allocate(&mut self, value: ssa::ValueId, size: usize) -> Location {
        self.offset += size;
        let location = Location::StackOffset(self.offset);
        self.locations.insert(value, location);
        return location;
    }

    fn is_primitive(&self, value: ssa::ValueId) -> bool {
        match &self.ir.values[value].kind {
            ssa::ValueKind::Int(_) 
            | ssa::ValueKind::Float(_) 
            | ssa::ValueKind::Bool(_) 
            // | ssa::ValueKind::String(_) 
            | ssa::ValueKind::Char(_)
            | ssa::ValueKind::Param{ .. } => true,
            _ => false
        }
    }

    fn gen_deps(&mut self, block: &mut BasicBlock, value: ssa::ValueId) {
        if !self.is_primitive(value) {
            self.create_asm(block, value);
        }
    }

    fn create_asm(&mut self, block: &mut BasicBlock, value: ssa::ValueId) {
        match &self.ir.values[value].kind {
            &ssa::ValueKind::Add { lhs, rhs } => {
                self.gen_deps(block, lhs);
                self.gen_deps(block, rhs);

                let out_lhs = self.get_location(lhs);
                let out_rhs = self.get_location(rhs);
                let size = self.get_size(value);

                let reg = Location::Register(Register::GPR {
                    kind: GPR::A,
                    size
                });

                block.push_inst(Asm::Mov(reg, out_lhs));
                block.push_inst(Asm::Add(reg, out_rhs));
                let push = self.stack_allocate(value, size);
                block.push_inst(Asm::Mov(push, reg));
            }
            &ssa::ValueKind::Mul { lhs, rhs } => {
                self.gen_deps(block, lhs);
                self.gen_deps(block, rhs);

                let out_lhs = self.get_location(lhs);
                let out_rhs = self.get_location(rhs);
                let size = self.get_size(value);

                let acc = Location::Register(Register::GPR {
                    kind: GPR::A,
                    size
                });

                let reg = Location::Register(Register::GPR {
                    kind: GPR::B,
                    size
                });

                block.push_inst(Asm::Mov(acc, out_lhs));
                block.push_inst(Asm::Mov(reg, out_rhs));
                block.push_inst(Asm::Mul(acc, reg));
                let push = self.stack_allocate(value, size);
                block.push_inst(Asm::Mov(push, acc));
            }
            ssa::ValueKind::Call { name, args } => {
                // push instructions
                let mut total: usize = 0;
                for &arg in args {
                    self.gen_deps(block, arg);
                    let loc = Location::Immediate(self.ssa_val_to_prim(arg));
                    block.push_inst(Asm::Push(loc));
                    total += self.get_size(arg);
                }

                // call, pop args 
                block.push_inst(Asm::Call(name.to_owned()));
                block.push_inst(Asm::Add(
                    Location::Register(Register::RBP), 
                    Location::Immediate(Immediate::Int(total as i64))
                ));
            }
            &ssa::ValueKind::Ret { value } => {
                self.gen_deps(block, value);

                let reg = Location::Register(Register::GPR { 
                    kind: GPR::A,
                    size: self.get_size(value) 
                });

                block.push_inst(Asm::Mov(reg, self.get_location(value)));
                block.push_inst(Asm::Pop(Location::Register(Register::RBP)));
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

    pub fn get_blocks(&mut self) -> Vec<BasicBlock> {
        std::mem::take(&mut self.blocks)
    }
}

