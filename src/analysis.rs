use crate::parser;
use std::cmp::max;
use std::collections::HashMap;
use std::vec::Vec;

fn variant_eq(a: &parser::Type, b: &parser::Type) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

#[derive(Debug, Clone)]
pub enum Symbol {
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
        // methods: HashMap<String, Symbol>, // Symbol::Function
        public: bool,
        typedef: parser::Type,
    },
}

trait Type {
    fn get_type(&self) -> parser::Type;
    // fn get_size(&self) -> Option<usize>;
}

// variable type must be known when you call this method
impl Type for Symbol {
    fn get_type(&self) -> parser::Type {
        match self {
            Symbol::Variable {
                variable_type,
                constant: _,
                public: _,
            } => {
                let Some(var_type) = variable_type else {
                    panic!("unknown type.");
                };

                return var_type.clone();
            }
            Symbol::Function {
                params: _,
                ret,
                public: _,
            } => ret.clone(),
            _ => panic!("can not get type."),
            // Symbol::Enum {
            //     varients: _,
            //     public: _,
            //     typedef,
            // } => Some((*typedef).clone()),
            // Symbol::Struct {
            //     members: _,
            //     // methods: _,
            //     public: _,
            //     typedef,
            // } => Some((*typedef).clone()),
        }
    }
}

pub struct Analyzer {
    symbol_tables: Vec<HashMap<String, Symbol>>,
    sizes: HashMap<String, usize>,
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
        // parser::Statement::EnumDeclaration {
        //     name,
        //     varients,
        //     public,
        // } => (
        //     name.clone(),
        //     Symbol::Enum {
        //         varients,
        //         public,
        //         typedef: parser::Type::Typedef(name),
        //     },
        // ),
        parser::Statement::StructDeclaration {
            name,
            members,
            // methods,
            public,
        } => {
            // TODO: these two symbol tables need to be updated to check for ODR,
            // should be much simpler cause they only require a check on their current scope
            let mut member_table: HashMap<String, Symbol> = HashMap::new();
            for member in members {
                add_symbol(&mut member_table, *member);
            }

            let mut method_table: HashMap<String, Symbol> = HashMap::new();
            // for method in methods {
            //     add_symbol(&mut method_table, *method);
            // }

            (
                name.clone(),
                Symbol::Struct {
                    members: member_table,
                    // methods: method_table,
                    public,
                    typedef: parser::Type::Typedef(name),
                },
            )
        }
        _ => panic!("can not create global symbol with statement"),
    }
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            symbol_tables: Vec::new(),
            sizes: HashMap::new(),
        }
    }

    fn add_table(&mut self) {
        self.symbol_tables.push(HashMap::new());
    }

    fn remove_table(&mut self) {
        _ = self.symbol_tables.pop();
    }

    fn get_current_table(&mut self) -> &mut HashMap<String, Symbol> {
        let i = self.symbol_tables.len() - 1;
        &mut self.symbol_tables[i]
    }

    fn check_definition(&self, identifier: String) -> bool {
        let len = self.symbol_tables.len();
        for i in (0..len).rev() {
            if let Some(_) = self.symbol_tables[i].get(&identifier) {
                return true;
            }
        }

        return false;
    }

    fn get_symbol(&self, identifier: String) -> Symbol {
        let len = self.symbol_tables.len();
        for i in (0..len).rev() {
            if let Some(s) = self.symbol_tables[i].get(&identifier) {
                return s.clone();
            }
        }

        panic!("unrecognized identifier");
    }

    fn add_symbol(&mut self, statement: parser::Statement) {
        let (name, symbol) = create_symbol_entry(statement);
        if self.check_definition(name.clone()) {
            panic!("identifier {} is already declared.", name);
        }

        self.get_current_table().insert(name, symbol);
    }
    
    // only works with numbers
    // TODO: UInt
    fn get_common_type(&mut self, lhs: parser::Type, rhs: parser::Type) -> parser::Type {
        match lhs {
            parser::Type::Int(lhs_int_size) => {
                match rhs {
                    parser::Type::Int(rhs_int_size) => return parser::Type::Int(max(lhs_int_size, rhs_int_size)),
                    parser::Type::Float(rhs_float_size) => return parser::Type::Float(rhs_float_size),
                    _ => {}
                }
            }
            parser::Type::Float(lhs_float_size) => {
                match rhs {
                    parser::Type::Int(_) => return parser::Type::Float(lhs_float_size),
                    parser::Type::Float(rhs_float_size) => return parser::Type::Float(max(lhs_float_size, rhs_float_size)),
                    _ => {}
                }
            }
            _ => {}
        };

        panic!("incompatible types.");
    }

    fn get_type(&mut self, expr: parser::Expression) -> parser::Type {
        match expr {
            parser::Expression::Null => parser::Type::Void,
            parser::Expression::Int(_) => parser::Type::Int(4),
            parser::Expression::Float(_) => parser::Type::Float(8),
            parser::Expression::String(_) => unimplemented!(),
            parser::Expression::Char(_) => parser::Type::Char,
            parser::Expression::Bool(_) => parser::Type::Bool,
            parser::Expression::Identifier(val) => {
                let s = self.get_symbol(val);
                return s.get_type();
            }
            parser::Expression::Unary { operator, member } => {
                match operator {
                    parser::Operator::BitwiseNot => parser::Type::Int(4),
                    parser::Operator::LogicalNot => parser::Type::Bool,
                    parser::Operator::Reference => parser::Type::Int(8),
                    parser::Operator::Dereference => {
                        self.get_type(*member)
                    }
                    _ => panic!("unexpected operator in unary expression. found: {:?}", operator),
                }
            }
            parser::Expression::Binary { lhs, operator, rhs } => {
                let lhs_type = self.get_type(*lhs); 
                let rhs_type = self.get_type(*rhs); 

                let boolean_expr = match operator {
                    parser::Operator::LogicalOr
                    | parser::Operator::LogicalAnd
                    | parser::Operator::Equal
                    | parser::Operator::NotEqual
                    | parser::Operator::LessThan
                    | parser::Operator::GreaterThan
                    | parser::Operator::LessThanEqual
                    | parser::Operator::GreaterThanEqual => true,
                    _ => false,
                };

                if boolean_expr {
                    // TODO: all equality operations, however, must be between ints
                    // TODO: maybe let null be an int?
                    return parser::Type::Bool;
                }

                if lhs_type == rhs_type {
                    return lhs_type;
                }

                return self.get_common_type(lhs_type, rhs_type);
            }
            parser::Expression::Assignment { identifier, value } => {
                let lhs_type = self.get_type(*identifier);
                let rhs_type = self.get_type(*value);

                if lhs_type == rhs_type {
                    return lhs_type;
                }

                return self.get_common_type(lhs_type, rhs_type);
            }
            parser::Expression::FunctionCall { identifier, args: _ } => self.get_type(*identifier),
            parser::Expression::ArrayAccess { identifier, index: _ } => self.get_type(*identifier),
            // maybe even just implement this as parser desugaring? like:
            // a = [1, 2, 3]; into
            // a[0] = 1;
            // a[1] = 2;
            // a[2] = 3;
            parser::Expression::ArrayConstructor { values } => {
                unimplemented!();
            }
            // TODO: lookup
            parser::Expression::StructMember { identifier, val } => {
                unimplemented!();
            }
            parser::Expression::StructConstructor { identifier, members } => {
                todo!();
            }
        }
    }

    fn analyze(&mut self, statement: parser::Statement, function_return_type: Option<parser::Type>) {
        match statement {
            parser::Statement::FunctionDeclaration { 
                name: _, 
                return_type, 
                parameters: _, 
                body, 
                public: _ 
            } => {
                // self.add_table();
                // for param in parameters {
                //     self.add_symbol(*param);
                // }
                self.analyze(*body, Some(return_type)); 
                // self.remove_table();
            }
            parser::Statement::Return { value } => {
                let Some(expected_type) = function_return_type else {
                    panic!("can not return outside of a function");
                };
                
                let value_type = self.get_type(*value);
                if !variant_eq(&expected_type, &value_type) {
                    panic!("function return type {:?}, found {:?}", expected_type, value_type);
                }
            }
            parser::Statement::Block(statements) => {
                self.add_table();
                for statement in statements {
                    self.analyze(*statement, function_return_type.clone());
                }
                self.remove_table();
            }
            parser::Statement::VariableDeclaration { 
                identifier, 
                mut variable_type, 
                initial_value, 
                constant, 
                public, 
                global 
            } => {
                if let Some(rhs) = initial_value.clone() {
                    let rhs_type = self.get_type(*rhs);
                    if let Some(var_type) = variable_type.clone() {
                        if !variant_eq(&rhs_type, &var_type) {
                            panic!("type of lhs does not match rhs.");
                        }
                    } else {
                        variable_type = Some(rhs_type);
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

                self.add_symbol(s);
            }
            parser::Statement::IfStatement { condition, block, alt } => {
                let condition_type = self.get_type(*condition);
                if !variant_eq(&condition_type, &parser::Type::Bool) {
                    panic!("condition must contain boolean expression.");
                }

                self.analyze(*block, function_return_type.clone());
                if let Some(else_block) = alt {
                    self.analyze(*else_block, function_return_type);
                }
            }
            parser::Statement::WhileStatement { condition, block } => {
                let condition_type = self.get_type(*condition);
                if !variant_eq(&condition_type, &parser::Type::Bool) {
                    panic!("condition must contain boolean expression.");
                }

                self.analyze(*block, function_return_type);
            }
            parser::Statement::ExpressionStatement(expr) => {
                self.get_type(*expr);
            }
            parser::Statement::ParseError => panic!("error from parser"),
            parser::Statement::Program(_) => panic!("derecated"),
            // TODO: almost want this to just be a compiler expansion / desugaring too
            parser::Statement::EnumDeclaration { name, varients, public } => unimplemented!(),
            parser::Statement::Member { name, t, public } => unimplemented!(),
            parser::Statement::StructDeclaration { name, members, public } => unimplemented!(),
            parser::Statement::Parameter { name: _, t: _ } => unreachable!(),
        }
    }

    pub fn get_globals(&mut self, ast: Vec<parser::Statement>) -> HashMap<String, Symbol> {
        self.add_table();
        for statement in ast {
            self.add_symbol(statement);     
        }

        return self.symbol_tables[0].clone();
    }

    // does semantic analysis too
    pub fn verify(&mut self, ast: Vec<parser::Statement>) -> HashMap<String, usize> {
        for statement in ast {
            self.analyze(statement, None);
        }

        return self.sizes.clone();
    }
}
