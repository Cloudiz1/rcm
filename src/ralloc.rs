use std::collections::HashSet;
use crate::ssa::{BlockId, IR};

struct Postorder<'a> {
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

fn intersect(b1: BlockId, b2: BlockId, doms: &Vec<Option<BlockId>>, pos: &Vec<usize>) -> BlockId {
    let mut f1 = b1;
    let mut f2 = b2;
    while f1 != f2 {
        while pos[f1] < pos[f2] {
            f1 = doms[f1].expect("ralloc::intersect unwrapped None");
        }
        while pos[f2] < pos[f1] {
            f2 = doms[f2].expect("ralloc::intersect unwrapped None");
        }
    }

    return f1;
}

pub fn create_dom_tree(ir: &IR) -> Vec<BlockId> {
    let mut postorder = Postorder::new(ir);
    let mut nodes = postorder.postorder();
    let pos: Vec<usize> = (0..ir.blocks.len()).map(|i| {
        nodes.iter().position(|x| *x == i).expect("could not find pos in RPO")
    }).collect();

    nodes.pop();

    let mut doms: Vec<Option<BlockId>> = vec![None; ir.blocks.len()];
    doms[ir.entry] = Some(ir.entry); // each node is its own dominator

    let mut changed: bool = true;
    while changed {
        changed = false;
        for b in nodes.iter().rev() {
            let mut new_idom = ir.blocks[*b].predecessors[0];

            for p in &ir.blocks[*b].predecessors[1..] {
                if doms[*p].is_some() {
                    new_idom = intersect(*p, new_idom, &doms, &pos);
                }
            }
            if doms[*b] != Some(new_idom) {
                doms[*b] = Some(new_idom);
                changed = true;
            }
        }
    }

    doms.into_iter().collect::<Option<Vec<BlockId>>>()
        .expect("ralloc::create_dom_tree panicked")
}
