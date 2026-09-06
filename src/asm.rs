use crate::{
    codegen::{Asm, BasicBlock},
    ssa,
};
use std::fmt::Write;
use std::fs::File;

type Counter = usize;

pub struct AsmGenerator {
    counter: Counter,
    id_lookup: Vec<ssa::BlockId>,
    output: Vec<String>,
    kinds: Vec<ssa::BlockKind>,
    block: String,
}

impl AsmGenerator {
    pub fn new() -> Self {
        Self {
            counter: 0,
            id_lookup: Vec::new(),
            output: Vec::new(),
            kinds: Vec::new(),
            block: String::new(),
        }
    }

    fn create_inst(&mut self, inst: Asm) {
        match inst {
            Asm::Mov(l, r)
            | Asm::Add(l, r)
            | Asm::Sub(l, r)
            | Asm::Mul(l, r)
            | Asm::Div(l, r) => {
                let inst = match inst {
                    Asm::Mov(_, _) => "mov",
                    Asm::Add(_, _) => "add",
                    Asm::Sub(_, _) => "sub",
                    Asm::Mul(_, _) => "mul",
                    Asm::Div(_, _) => "div",
                    _ => unreachable!(),
                };

                write!(self.block, "  {} {}, {}\n", inst, l, r).unwrap();
            }
            Asm::Push(l) => write!(self.block, "  push {}\n", l).unwrap(),
            Asm::Pop(l)  => write!(self.block, "  pop {}\n", l).unwrap(),
            Asm::Jmp(dest) => write!(self.block, "  jmp L{}:\n", self.id_lookup[dest]).unwrap(),
            Asm::Ret => write!(self.block, "  ret\n").unwrap(),
            Asm::Call(name) => write!(self.block, "  call {}\n", name).unwrap(),
            _ => {
                dbg!(inst);
                unimplemented!();
            }
        }
    }

    fn emit_asm(&mut self, block: BasicBlock) {
        write!(self.block, "L{}: ; {} \n", self.counter, block.label).unwrap();
        self.id_lookup.push(block.id);
        self.counter += 1;

        for inst in block.instructions {
            self.create_inst(inst);
        }

        self.output.push(std::mem::take(&mut self.block));
        self.kinds.push(block.kind);
    }

    pub fn create_asm(mut self, blocks: Vec<BasicBlock>) {
        use std::io::Write; // avoid std::fmt::Write conflict
        for block in blocks {
            self.emit_asm(block);
        }

        let mut last_exit: usize = 0;
        for (i, kind) in self.kinds.iter().enumerate() {
            match kind {
                ssa::BlockKind::FunctionExit => last_exit = i,
                ssa::BlockKind::FunctionEntry => self.output.swap(i, last_exit),
                _ => {}
            }
        }
        let output = self.output.join("");

        eprintln!("{}", output);
        File::create("out.asm")
            .expect("error opening output file")
            .write(output.as_bytes())
            .expect("error writing output");
    }
}

