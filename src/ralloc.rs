use std::collections::HashSet;
use crate::ssa::{BlockId, IR};

pub struct Postorder<'a> {
    ir: &'a IR,
    visited: HashSet<BlockId>,
    values: Vec<BlockId>,
}

impl<'a> Postorder<'a> {
    pub fn new(ir: &'a IR) -> Self {
        Self {
            ir,
            visited: HashSet::new(),
            values: Vec::new(),
        }
    }

    fn dfs(&mut self, entry: BlockId) {
        self.visited.insert(entry);

        for s in &self.ir.blocks[entry].successors {
            if self.visited.get(s).is_none() {
                self.dfs(*s);
            }
        }

        self.values.push(entry);
    }

    pub fn postorder(&mut self) -> Vec<BlockId> {
        self.dfs(self.ir.entry);
        return std::mem::take(&mut self.values);
    }
}
