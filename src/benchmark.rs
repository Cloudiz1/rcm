use std::time::{Instant, Duration};

pub struct Benchmarks {
    names: Vec<&'static str>,
    instances: Vec<Instant>,
    elapsed: Vec<Duration>,
}

impl Benchmarks {
    pub fn new() -> Self {
        Self {
            names: Vec::new(),
            instances: Vec::new(),
            elapsed: Vec::new(),
        }
    }

    pub fn add(&mut self, name: &'static str) {
        if self.instances.len() != self.elapsed.len() {
            panic!("can not start a new instance without ending the old one");
        }

        self.names.push(name);
        self.instances.push(Instant::now());
    }

    pub fn end(&mut self) {
        self.elapsed.push(self.instances[self.instances.len() - 1].elapsed());
    }

    pub fn show(&self) {
        let width = {
            let mut largest = 0;
            for name in &self.names {
                if name.len() > largest {
                    largest = name.len();
                }
            }

            largest
        };

        println!("");
        println!("Benchmarks (in micros): ");
        println!("-------------------------");
        for (name, elapsed) in self.names.iter().zip(self.elapsed.clone()) {
            println!(" {:>width$}: {}", name, elapsed.as_micros(), width=width);
        }
    }
}
