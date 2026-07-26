use crate::lexer;
use std::hash::Hash;
use std::collections::HashSet;
use std::marker::PhantomData;

pub fn print_error(token: &lexer::DebugToken, lines: &Vec<String>, src_path: &String, msg: &str) {
    println!("at {}:{}", src_path, token.line_number + 1);
    println!("{} | {}", token.line_number + 1, lines[token.line_number]);
    for _ in 0..token.column {
        print!(" ");
    }

    let line_len = (token.line_number + 1).to_string().len();
    for _ in 0..line_len {
        print!(" ");
    }

    println!("   ^ {}", msg);
    println!("");
}

pub struct Postorder<'a, T, F> {
    stack: Vec<(T, bool)>,
    visited: HashSet<T>,
    get_children: F,
    phantom: PhantomData<&'a T>
}

impl<'a, T, F> Postorder<'a, T, F>
where
    T: Eq + Clone + Hash + 'a,
    F: FnMut(&T) -> &'a [T],
{
    pub fn new(root: T, get_children: F) -> Self {
        Self {
            stack: vec![(root, false)],
            visited: HashSet::new(),
            get_children,
            phantom: PhantomData,
        }
    }
}

impl<'a, T, F> Iterator for Postorder<'a, T, F>
where
    T: Eq + Copy + Hash + 'a,
    F: FnMut(&T) -> &'a [T],
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((node, expanded)) = self.stack.pop() {
            if expanded {
                return Some(node);
            }

            if self.visited.contains(&node) {
                continue;
            }

            self.visited.insert(node);
            self.stack.push((node, true));

            for child in (self.get_children)(&node).into_iter().rev() {
                if !self.visited.contains(child) {
                    self.stack.push((*child, false));
                }
            }
        }
        None
    }
}

pub trait TraversalExt: Sized {
    fn postorder<'a, F>(self, get_children: F) -> Postorder<'a, Self, F>
    where
        Self: Eq + Hash + Clone,
        F: FnMut(&Self) -> &'a [Self],
    {
        Postorder::new(self, get_children)
    }
}

impl<T: Eq + Hash + Clone> TraversalExt for T {}
