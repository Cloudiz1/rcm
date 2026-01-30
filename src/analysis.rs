// TODO:
// i want to rebuild the ast that came out of the parser
// this ast should have types built in
// id like to remove all Option<T>, feels dumb to have uncertainty in codegen
// this also leads to a good time for compiler expansions/desugaring
// id like to collect a symbol table too

use crate::parser;
use std::collections::HashMap;

enum Symbol {
    Function {
        public: bool,
        return_type: parser::Type,
        params: Vec<parser::Type>
    },
    Variable {
        public: bool,
        constant: bool,
        variable_type: parser::Type,
        // value: parser::Expression,
    },
    Struct {
        public: bool,
        members: HashMap<String, Symbol>,
        methods: HashMap<String, Symbol>,
    },
    Member(parser::Type),
}

fn create_symbol_entry(statement: parser::Statement) -> (String, Symbol) {
    match statement {
        parser::Statement::FunctionDeclaration { 
            name, 
            return_type, 
            parameters, 
            body, 
            public 
        } => {
            let mut params: Vec<parser::Type> = Vec::new();
            for param in parameters {
                match *param {
                    parser::Statement::Parameter { name, t } => {
                        params.push(t);
                    }
                    _ => unreachable!(),
                }
            }

            return (
                name, 
                Symbol::Function { 
                    public, 
                    return_type, 
                    params 
                }
            );
        }
        parser::Statement::StructDeclaration { 
            name, 
            members, 
            methods, 
            public 
        } => {
            let mut symbol_members: HashMap<String, Symbol> = HashMap::new();
            for member in members {
                let (name, symbol) = create_symbol_entry(*member);
                check_definition_single(&symbol_members, &name);
                add_symbol(&mut symbol_members, name, symbol);
            }

            let mut symbol_methods: HashMap<String, Symbol> = HashMap::new();
            for method in methods {
                let (name, symbol) = create_symbol_entry(*method);
                check_definition_single(&symbol_members, &name);
                add_symbol(&mut symbol_methods, name, symbol);
            }

            return (
                name,
                Symbol::Struct { 
                    public, 
                    members: symbol_members, 
                    methods: symbol_methods,
                }
            )
        }
        parser::Statement::Member { 
            name, 
            t, 
            public 
        } => {
            return (name, Symbol::Member(t));
        }
        parser::Statement::VariableDeclaration { 
            identifier, 
            variable_type, 
            initial_value, 
            constant, 
            public, 
            global 
        } => {
            let t = match variable_type {
                Some(v) => v,
                None => parser::Type::Void, // TODO: type inference!
            };

            return (
                identifier,
                Symbol::Variable { 
                    public, 
                    constant, 
                    variable_type: t,
                }
            )
        }
        _ => panic!("can not create symbol from: {:?}", statement),
    }
}

fn check_definition_single(table: &HashMap<String, Symbol>, identifier: &String) -> bool {
    if let Some(_) = table.get(identifier) {
        return true;
    }
     return false;
}

fn check_definition(tables: &Vec<HashMap<String, Symbol>>, identifier: &String) -> bool {
    let len = tables.len();
    for i in (0..len).rev() {
        if check_definition_single(&tables[i], identifier) {
            return true;
        }
    }

    return false;
}

fn add_symbol(table: &mut HashMap<String, Symbol>, name: String, symbol: Symbol) {
    // let (name, symbol) = create_symbol_entry(statement);
    // if check_definition(tables, &name) {
    //     panic!("identifier {} is already declared.", name);
    // }

    // let index = tables.len() - 1;
    // let table = &mut tables[index];
    table.insert(name, symbol);
}

pub struct Analyzer {
    tables: Vec<HashMap<String, Symbol>>,
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            tables: Vec::new(),
        } 
    }

    fn add_table(&mut self) {
        self.tables.push(HashMap::new());
    }

    fn remove_table(&mut self) {
        _ = self.tables.pop();
    }

    fn get_current_table(&mut self) -> &mut HashMap<String, Symbol> {
        let i = self.tables.len() - 1;
        &mut self.tables[i]
    }

    fn add_symbol(&mut self, statement: parser::Statement) {
        let (name, symbol) = create_symbol_entry(statement);
        check_definition(&self.tables, &name);
        add_symbol(self.get_current_table(), name, symbol);
    }

    fn get_globals(&mut self, ast: Vec<parser::Statement>) {
        self.add_table();
        for statement in ast {
            self.add_symbol(statement);
            // add_symbol(&mut self.tables, statement);
        }
    }

    pub fn analyze(&mut self, ast: Vec<parser::Statement>) {
        self.get_globals(ast);
    }
}
