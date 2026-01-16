use crate::parser;
use std::collections::HashMap;
use std::hash::Hash;
use std::vec::Vec;

#[derive(Debug)]
enum Symbol {
    Function {
        params: Vec<parser::Type>,
        ret: parser::Type,
        public: bool,
    },
    Variable {
        variable_type: Option<parser::Type>,
        constant: bool,
        public: bool,
    },
    Enum {
        varients: Vec<String>,
        public: bool,
    },
    Struct {
        members: HashMap<String, Symbol>, // Symbol::Variable
        methods: HashMap<String, Symbol>, // Symbol::Function
        public: bool,
    },
}

fn add_symbol_entry(table: &mut HashMap<String, Symbol>, statement: parser::Statement) {
    let (name, symbol) = create_symbol_entry(statement);
    table.insert(name, symbol);
}

fn create_symbol_entry(statement: parser::Statement) -> (String, Symbol) {
    match statement {
        parser::Statement::FunctionDeclaration {
            name,
            return_type,
            parameters,
            body: _,
            public,
        } => (
            name,
            Symbol::Function {
                params: parameters.into_iter().map(|x| x.t).collect(),
                ret: return_type,
                public,
            },
        ),
        parser::Statement::VariableDeclaration {
            identifier,
            variable_type,
            initial_value: _,
            constant,
            public,
        } => (
            identifier,
            Symbol::Variable {
                variable_type,
                constant,
                public,
            },
        ),
        parser::Statement::Member { name, t, public } => (
            name,
            Symbol::Variable {
                variable_type: Some(t),
                constant: false,
                public,
            },
        ),
        parser::Statement::EnumDeclaration {
            name,
            varients,
            public,
        } => (name, Symbol::Enum { varients, public }),
        parser::Statement::StructDeclaration {
            name,
            members,
            methods,
            public,
        } => {
            let mut member_table: HashMap<String, Symbol> = HashMap::new();
            for member in members {
                add_symbol_entry(&mut member_table, *member);
            }

            let mut method_table: HashMap<String, Symbol> = HashMap::new();
            for method in methods {
                add_symbol_entry(&mut method_table, *method);
            }

            return (
                name,
                Symbol::Struct {
                    members: member_table,
                    methods: method_table,
                    public,
                },
            );
        }
        _ => unreachable!("can not create symbol out of something other than a declaration."),
    }
}

pub struct TAC {
    globals: HashMap<String, Symbol>,
    tables: Vec<HashMap<String, Symbol>>,
}

impl TAC {
    // TODO: should generate globals!
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            tables: Vec::new(),
        }
    }

    pub fn generate(&mut self, input: Vec<Box<parser::Statement>>) {
        for statement in input {
            add_symbol_entry(&mut self.globals, *statement);
        }

        println!("{:#?}", self.globals);
    }
}
