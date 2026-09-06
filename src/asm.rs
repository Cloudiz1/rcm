use crate::{
    codegen::{Asm, BasicBlock},
    ssa,
};
use std::fmt::Write;
use std::fs::File;


#[derive(Debug, Clone)]
pub enum AsmSize {
    BYTE,
    WORD,
    DWORD,
    QWORD,
}

impl AsmSize {
    pub fn new(size: usize) -> Self {
        match size {
            1 => Self::BYTE,
            2 => Self::WORD,
            4 => Self::DWORD,
            8 => Self::QWORD,
            _ => panic!("invalid default size in Asm")
        }
    }
}

impl std::fmt::Display for AsmSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AsmSize::BYTE => write!(f, "byte"),
            AsmSize::WORD => write!(f, "word"),
            AsmSize::DWORD => write!(f, "dword"),
            AsmSize::QWORD => write!(f, "qword"), // 64 bit is default, it can actually be omitted
        }
    }
}

type Counter = usize;
pub struct AsmGenerator {
    counter: Counter,
    main: Counter,
    id_lookup: Vec<ssa::BlockId>,
    output: Vec<String>,
    kinds: Vec<ssa::BlockKind>,
    block: String,
}

impl AsmGenerator {
    pub fn new() -> Self {
        Self {
            counter: 0,
            main: 0,
            id_lookup: Vec::new(),
            output: Vec::new(),
            kinds: Vec::new(),
            block: String::new(),
        }
    }

    fn create_inst(&mut self, inst: Asm) {
        match inst {
            Asm::Add(l, r)
            | Asm::Sub(l, r)
            | Asm::Mul(l, r)
            | Asm::Div(l, r) => {
                let inst = match inst {
                    Asm::Add(_, _) => "add",
                    Asm::Sub(_, _) => "sub",
                    Asm::Mul(_, _) => "mul",
                    Asm::Div(_, _) => "div",
                    _ => unreachable!(),
                };

                write!(self.block, "\t{} {}, {}\n", inst, l, r).unwrap();
            }
            Asm::Mov(l, r, size) => {
                write!(self.block, "\tmov {} {}, {}\n", AsmSize::new(size), l, r).unwrap()
            }
            Asm::Push(l) => write!(self.block, "\tpush {}\n", l).unwrap(),
            Asm::Pop(l)  => write!(self.block, "\tpop {}\n", l).unwrap(),
            Asm::Jmp(dest) => write!(self.block, "\tjmp L{}:\n", self.id_lookup[dest]).unwrap(),
            Asm::Ret => {
                write!(self.block, "\tmov rsp, rbp\n").unwrap();
                write!(self.block, "\tret\n").unwrap()
            }
            Asm::Call(name) => write!(self.block, "\tcall {}\n", name).unwrap(),
            _ => {
                dbg!(inst);
                unimplemented!();
            }
        }
    }

    fn emit_asm(&mut self, block: BasicBlock) {
        if matches!(block.kind, ssa::BlockKind::FunctionEntry) {
            write!(self.block, "{}:\n", block.label).unwrap();
        } else {
            write!(self.block, "L{}: ; {} \n", self.counter, block.label).unwrap();
            self.id_lookup.push(block.id);
            self.counter += 1;
        }

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

        let start: String = "\
        global _start \
        \n_start: \
        \n\tcall main \
        \n\tmov rdi, rax \
        \n\tmov rax, 60 \
        \n\tsyscall\n".to_owned(); 

        let output = start + &self.output.join("");

        eprintln!("{}", output);
        File::create("build/out.asm")
            .expect("error opening output file")
            .write(output.as_bytes())
            .expect("error writing output");
    }
}

