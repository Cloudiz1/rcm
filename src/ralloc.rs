use std::collections::HashSet;
use crate::ssa::{BlockId, IR};
use crate::util::TraversalExt;

// this uses the CHK dom tree algorithm
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

pub fn find_idoms(ir: &IR) -> Vec<BlockId> {
    let mut nodes = ir.entry.postorder(|x| &ir.blocks[*x].successors).collect::<Vec<_>>();
    let pos: Vec<usize> = (0..ir.blocks.len()).map(|i| {
        nodes.iter().position(|x| *x == i).expect(&std::format!("could not find pos for {}", i))
    }).collect();

    nodes.pop();

    let mut doms: Vec<Option<BlockId>> = vec![None; ir.blocks.len()];
    doms[ir.entry] = Some(ir.entry); // each node is its own dominator

    let mut changed: bool = true;
    while changed {
        changed = false;
        for b in nodes.iter().rev() {
            let mut new_idom = ir.blocks[*b].predecessors[0];
            for &p in &ir.blocks[*b].predecessors[1..] {
                if doms[p].is_some() {
                    new_idom = intersect(p, new_idom, &doms, &pos);
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

struct DomTree {
    idoms: Vec<BlockId>,
    children: Vec<Vec<BlockId>>, // given that p is the idom of u, children[p] lists all blocks u
                                 // whos idom is p
    tin: Vec<BlockId>,
    tout: Vec<BlockId>
}

// impl DomTree {
//     pub fn new(idom: Vec<BlockId>, entry: BlockId) -> Self {
//         let mut children: Vec<Vec<BlockId>> = vec![Vec::new(); idom.len()];
//         for u in 0..idom.len() {
//             if u == entry { continue; }
//             let p = idom[u];
//             children[p].push(u);
//         }
//     }
//
//     fn dfs(entry: BlockId) {
//     }
// }

// fn backedges(ir: &IR) -> Vec<(BlockId, BlockId)> {
//     for u in ir.blocks {
//         for h in u.successors {
//         }
//     }
// }
