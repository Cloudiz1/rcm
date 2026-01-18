use crate::parser;
use std::collections::HashMap;
use std::vec::Vec;

#[derive(Debug, Clone)]
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

trait Type {
    fn get_type(&self) -> Option<parser::Type>;
}

impl Type for Symbol {
    fn get_type(&self) -> Option<parser::Type> {
        match self {
            Symbol::Variable {
                variable_type,
                constant: _,
                public: _,
            } => (*variable_type).clone(),
            Symbol::Function {
                params: _,
                ret,
                public: _,
            } => Some((*ret).clone()),
            _ => panic!("can not get type"),
        }
    }
}

fn add_symbol(table: &mut HashMap<String, Symbol>, statement: parser::Statement) {
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
            global,
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
                add_symbol(&mut member_table, *member);
            }

            let mut method_table: HashMap<String, Symbol> = HashMap::new();
            for method in methods {
                add_symbol(&mut method_table, *method);
            }

            (
                name,
                Symbol::Struct {
                    members: member_table,
                    methods: method_table,
                    public,
                },
            )
        }
        _ => panic!("can not create global symbol with statement"),
    }
}

pub struct TAC {
    tables: Vec<HashMap<String, Symbol>>,
    temporary_count: u32, // _t0
    label_count: u32,     // _L0
}

impl TAC {
    pub fn new() -> Self {
        Self {
            tables: Vec::new(),
            temporary_count: 0,
            label_count: 0,
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

    fn check_definition(&self, identifier: String) {
        let len = self.tables.len();
        for i in (0..len).rev() {
            if let Some(_) = self.tables[i].get(&identifier) {
                return;
            }
        }

        panic!("unrecognized identifier {}", identifier);
    }

    fn get_symbol(&self, identifier: String) -> Symbol {
        let len = self.tables.len();
        for i in (0..len).rev() {
            if let Some(val) = self.tables[i].get(&identifier) {
                return (*val).clone();
            }
        }

        // should never be called, should be checked before calling this function
        panic!("symbol not found.");
    }

    // fn get_symbol(&self, identifier: parser::Expression) -> Symbol {
    //     match identifier {
    //         parser::Expression::Identifier(val) => self.get_symbol_from_string(val),
    //         parser::Expression::FunctionCall {
    //             identifier,
    //             args: _,
    //         } => self.get_symbol(*identifier),
    //         parser::Expression::ArrayAccess {
    //             identifier,
    //             index: _,
    //         } => self.get_symbol(*identifier),
    //         _ => {
    //             dbg!(identifier);
    //             panic!("can not get symbol for expression.");
    //         }
    //     }
    // }

    fn handle_expression(&self, expr: parser::Expression) -> parser::Type {
        match expr {
            // parser::Expression::Null => {}
            // parser::Expression::Int(val) => {}
            // parser::Expression::Float(val) => {}
            // parser::Expression::String(val) => {}
            // parser::Expression::Char(val) => {}
            // parser::Expression::Bool(val) => {}
            parser::Expression::Identifier(val) => {
                self.check_definition(val.clone());
                if let Some(t) = self.get_symbol(val).get_type() {
                    return t;
                }

                // TODO: type inference
                panic!("can not infer type");
            }
            // parser::Expression::Binary { lhs, operator, rhs } => {
            //     self.handle_expression(*lhs);
            //     self.handle_expression(*rhs);
            // }
            parser::Expression::Unary { operator, member } => self.handle_expression(*member),
            parser::Expression::Postfix { lhs, operator } => self.handle_expression(*lhs),
            parser::Expression::Assignment { identifier, value } => {
                self.handle_expression(*identifier)
            }
            parser::Expression::FunctionCall { identifier, args } => {
                self.handle_expression(*identifier)
            }
            parser::Expression::ArrayAccess { identifier, index } => {
                self.handle_expression(*identifier)
            }
            _ => unimplemented!("unimplemented expression"),
        }
    }

    pub fn codegen(&mut self, statement: parser::Statement) {
        match statement {
            parser::Statement::Program(statements) => {
                self.add_table();
                for s in statements.clone() {
                    add_symbol(&mut self.get_current_table(), *s);
                }

                for s in statements {
                    self.codegen(*s);
                }
            }
            parser::Statement::FunctionDeclaration {
                name: _,
                return_type: _,
                parameters: _,
                body,
                public: _,
            } => {
                self.codegen(*body);
            }
            parser::Statement::Block(statements) => {
                self.add_table();
                for s in statements {
                    self.codegen(*s);
                }

                self.remove_table();
            }
            parser::Statement::VariableDeclaration {
                identifier,
                variable_type,
                initial_value,
                constant,
                public,
                global,
            } => {
                // just like, dont look at this too hard
                let s = parser::Statement::VariableDeclaration {
                    identifier,
                    variable_type,
                    initial_value,
                    constant,
                    public,
                    global,
                };
                add_symbol(self.get_current_table(), s);
            }
            parser::Statement::ExpressionStatement(s) => {
                self.handle_expression(*s);
            }
            _ => {}
        };
    }
}
