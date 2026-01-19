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
        typedef: parser::Type,
    },
    Struct {
        members: HashMap<String, Symbol>, // Symbol::Variable
        methods: HashMap<String, Symbol>, // Symbol::Function
        public: bool,
        typedef: parser::Type,
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
            Symbol::Enum {
                varients: _,
                public: _,
                typedef,
            } => Some((*typedef).clone()),
            Symbol::Struct {
                members: _,
                methods: _,
                public: _,
                typedef,
            } => Some((*typedef).clone()),
        }
    }
}

fn variant_eq(a: &parser::Type, b: &parser::Type) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

// TODO: Odr
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
        } => (
            name.clone(),
            Symbol::Enum {
                varients,
                public,
                typedef: parser::Type::Typedef(name),
            },
        ),
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
                name.clone(),
                Symbol::Struct {
                    members: member_table,
                    methods: method_table,
                    public,
                    typedef: parser::Type::Typedef(name),
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

    fn get_type(&self, expr: parser::Expression) -> parser::Type {
        match expr {
            parser::Expression::Null => parser::Type::Pointer(Box::new(parser::Type::Void)),
            parser::Expression::Int(_) => parser::Type::I32,
            parser::Expression::Float(_) => parser::Type::F64,
            parser::Expression::Char(_) => parser::Type::Char,
            parser::Expression::String(_) => parser::Type::Str,
            parser::Expression::Bool(_) => parser::Type::Bool,
            parser::Expression::Unary { operator, member } => {
                let t = self.get_type(*member);
                match operator {
                    parser::Operator::Reference => parser::Type::Pointer(Box::new(t)),
                    parser::Operator::Negate | parser::Operator::BitwiseNot => parser::Type::I32,
                    parser::Operator::LogicalNot => parser::Type::Bool,
                    parser::Operator::Dereference => t,
                    _ => unreachable!(),
                }
            }
            parser::Expression::Binary { lhs, operator, rhs } => {
                let tlhs = self.get_type(*lhs);
                let trhs = self.get_type(*rhs);

                match operator {
                    parser::Operator::Add
                    | parser::Operator::Subtract
                    | parser::Operator::Multiply
                    | parser::Operator::Divide
                    | parser::Operator::Modulus => {
                        if variant_eq(&tlhs, &trhs) {
                            return tlhs;
                        }

                        unimplemented!("needs basic type conversion");
                        // TODO: these should have the same types (or close to, some basic type
                        // inferences should be allowed imo)
                        // or maybe just the same for now cause im useless and stupid (except for
                        // int -> int and float -> float)
                    }
                    parser::Operator::BitwiseOr
                    | parser::Operator::BitwiseAnd
                    | parser::Operator::BitwiseXOr
                    | parser::Operator::BitwiseLeftShift
                    | parser::Operator::BitwiseRightShift => return parser::Type::I32,
                    parser::Operator::LogicalOr
                    | parser::Operator::LogicalAnd
                    | parser::Operator::Equal
                    | parser::Operator::NotEqual
                    | parser::Operator::LessThan
                    | parser::Operator::GreaterThan
                    | parser::Operator::LessThanEqual
                    | parser::Operator::GreaterThanEqual => return parser::Type::Bool,
                    _ => panic!("invalid binary expression"),
                }
            }
            // TODO: like the type inference would be nice thanks
            parser::Expression::Identifier(val) => self.get_symbol(val).get_type().unwrap(),
            parser::Expression::FunctionCall {
                identifier,
                args: _,
            } => match *identifier {
                parser::Expression::String(val) => self.get_symbol(val).get_type().unwrap(),
                _ => self.get_type((*identifier).clone()),
            },
            parser::Expression::ArrayAccess {
                identifier,
                index: _,
            } => self.get_type(*identifier),
            // parser::Expression::Assignment { identifier, value } => {
            //     let expected_type = self.get_type(*identifier);
            //     let rhs_type = self.get_type(*value);
            //     if variant_eq(&expected_type, &rhs_type) {
            //         return expected_type;
            //     }
            //
            //     panic!("expected type on lhs differs from rhs.");
            // }
            parser::Expression::ArrayConstructor { mut values } => {
                let mut t1 = parser::Type::Void;
                if let Some(a) = values.pop() {
                    t1 = self.get_type(*a);
                }

                for val in values {
                    let t2 = self.get_type(*val);
                    if !variant_eq(&t1, &t2) {
                        panic!("array can only have one data type.");
                    }
                }

                return t1;
            }
            parser::Expression::StructConstructor {
                identifier,
                members: _,
            } => self.get_symbol(identifier).get_type().unwrap(),
            // TODO: This shits like completely evil i think. I dont have a good way of checking if
            // this is like part of a struct i dont think and currently nothing reads from a
            // structs symbol table, so ill have to do some thinking
            parser::Expression::StructMember { identifier, val } => unimplemented!(),
            _ => unimplemented!(),
        }
    }

    fn handle_expression(&self, expr: parser::Expression) {
        match expr {
            parser::Expression::Assignment { identifier, value } => {
                println!("assigning");
                let expected_type = self.get_type(*identifier);
                let rhs_type = self.get_type(*value);
                if !variant_eq(&expected_type, &rhs_type) {
                    panic!("expected type on lhs differs from rhs.");
                }
            }
            _ => unimplemented!(),
        }
    }

    // fn handle_expression(&self, expr: parser::Expression) -> Option<parser::Type> {
    //     match expr {
    //         parser::Expression::Null => Some(parser::Type::Pointer(Box::new(parser::Type::Void))),
    //         parser::Expression::Int(val) => None,
    //         parser::Expression::Float(val) => None,
    //         parser::Expression::String(val) => Some(parser::Type::Str),
    //         parser::Expression::Char(val) => Some(parser::Type::Char),
    //         // parser::Expression::Bool(val) => {}
    //         parser::Expression::Identifier(val) => {
    //             self.check_definition(val.clone());
    //             if let Some(t) = self.get_symbol(val).get_type() {
    //                 return Some(t);
    //             }
    //
    //             // TODO: type inference
    //             panic!("can not infer type");
    //         }
    //         // parser::Expression::Binary { lhs, operator, rhs } => {
    //         //     self.handle_expression(*lhs);
    //         //     self.handle_expression(*rhs);
    //         // }
    //         parser::Expression::Unary { operator, member } => self.handle_expression(*member),
    //         parser::Expression::Postfix { lhs, operator } => self.handle_expression(*lhs),
    //         parser::Expression::Assignment { identifier, value } => {
    //             if let Some(t) = self.handle_expression(*identifier) {
    //                 if let Some(rhs) = self.handle_expression(*value) {
    //                     if !variant_eq(&t, &rhs) {
    //                         panic!("mismatched types in assignemnt.");
    //                     }
    //                 }
    //             }
    //
    //             let rhs = self.handle_expression(*value);
    //             // if variant_eq(t, rhs) {
    //             //     return None;
    //             // }
    //
    //             panic!("mismatched types.");
    //         }
    //         parser::Expression::FunctionCall { identifier, args } => {
    //             self.handle_expression(*identifier)
    //         }
    //         parser::Expression::ArrayAccess { identifier, index } => {
    //             self.handle_expression(*identifier)
    //         }
    //         _ => unimplemented!("unimplemented expression"),
    //     }
    // }

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
                mut variable_type,
                initial_value,
                constant,
                public,
                global,
            } => {
                if let Some(rhs) = initial_value.clone() {
                    let t = self.get_type(*rhs);
                    if let Some(var_type) = variable_type.clone() {
                        if !variant_eq(&t, &var_type) {
                            panic!("types of lhs do not match rhs");
                        }
                    } else {
                        variable_type = Some(t);
                    }
                } else {
                    let Some(_) = variable_type else {
                        panic!("variable declaration with no rhs requires a type.");
                    };
                }

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
