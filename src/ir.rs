use crate::parser;
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
        // typedef: parser::Type,
    },
    Struct {
        members: HashMap<String, Symbol>, // Symbol::Variable
        // methods: HashMap<String, Symbol>, // Symbol::Function
        public: bool,
        // typedef: parser::Type,
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

pub struct Generator {
    symbol_tables: Vec<HashMap<String, Symbol>>,
    sizes: HashMap<parser::Type, usize>,
    l_count: usize,
    t_count: usize,
    out: String,
}

fn get_operator(operator: parser::Operator) -> &'static str {
    match operator {
        parser::Operator::Add => "+",
        parser::Operator::Subtract => "-",
        parser::Operator::Multiply => "*",
        parser::Operator::Divide => "/",
        parser::Operator::Modulus => "%",
        parser::Operator::BitwiseOr => "|",
        parser::Operator::BitwiseXOr => "^",
        parser::Operator::BitwiseAnd => "&",
        parser::Operator::BitwiseLeftShift => "<<",
        parser::Operator::BitwiseRightShift => ">>",
        parser::Operator::LogicalOr => "||",
        parser::Operator::LogicalAnd => "&&",
        parser::Operator::Equal => "==",
        parser::Operator::NotEqual => "!=",
        parser::Operator::LessThan => "<",
        parser::Operator::GreaterThan => ">",
        parser::Operator::LessThanEqual => "<=",
        parser::Operator::GreaterThanEqual => ">=",
        _ => panic!("unrecognized operaator"),
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

            // let mut method_table: HashMap<String, Symbol> = HashMap::new();
            // for method in methods {
            //     add_symbol(&mut method_table, *method);
            // }

            (
                name.clone(),
                Symbol::Struct {
                    members: member_table,
                    // methods: method_table,
                    public,
                    // typedef: parser::Type::Typedef(name),
                },
            )
        }
        _ => panic!("can not create global symbol with statement"),
    }
}

impl Generator {
    pub fn new() -> Self {
        let mut sizes: HashMap<parser::Type, usize> = HashMap::new();
        sizes.insert(parser::Type::I8, 1);
        sizes.insert(parser::Type::U8, 1);
        sizes.insert(parser::Type::I16, 2);
        sizes.insert(parser::Type::U16, 2);
        sizes.insert(parser::Type::I32, 4);
        sizes.insert(parser::Type::U32, 4);
        sizes.insert(parser::Type::I64, 8);
        sizes.insert(parser::Type::U64, 8);
        sizes.insert(parser::Type::F16, 2);
        sizes.insert(parser::Type::F32, 4);
        sizes.insert(parser::Type::F64, 8);
        sizes.insert(parser::Type::Bool, 1);
        sizes.insert(parser::Type::Char, 4);
        
        Self {
            symbol_tables: Vec::new(),
            sizes,
            l_count: 0,
            t_count: 0,
            out: String::new(),
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

    fn calculate_symbol_size(&mut self, symbol: Symbol) -> usize {
        match symbol {
            Symbol::Struct { members, public: _ } => {
                let mut sum: usize = 0;
                for member in members.values().clone().collect::<Vec<&Symbol>>() {
                    sum += self.calculate_symbol_size(member.clone()) 
                }

                return sum;
            }
            Symbol::Variable { variable_type, constant: _, public : _ } => {
                let Some(member_type) = variable_type else {
                    unreachable!();
                };

                return self.get_size(member_type);
            }
            _ => panic!("can not calculate symbol size"),
        }
    }

    // also inserts sizes of unknown structs
    fn get_size(&mut self, t: parser::Type) -> usize {
        if let Some(size) = self.sizes.get(&t.clone()) {
            return size.clone();
        }

        match t.clone() {
            parser::Type::Typedef(identifier) => {
                let symbol = self.get_symbol(identifier);
                let new_size = self.calculate_symbol_size(symbol);
                self.sizes.insert(t, new_size);
                return new_size;
            }
            _ => panic!("unknown size"),
        }
    }
    
    // TODO: UInt
    //
    // only works with numbers
    // fn get_common_type(&self, lhs: parser::Type, rhs: parser::Type) -> parser::Type {
    //     match lhs {
    //         parser::Type::Int(lhs_int_size) => {
    //             match rhs {
    //                 parser::Type::Int(rhs_int_size) => return parser::Type::Int(max(lhs_int_size, rhs_int_size)),
    //                 parser::Type::Float(rhs_float_size) => return parser::Type::Float(rhs_float_size),
    //                 _ => {}
    //             }
    //         }
    //         parser::Type::Float(lhs_float_size) => {
    //             match rhs {
    //                 parser::Type::Int(_) => return parser::Type::Float(lhs_float_size),
    //                 parser::Type::Float(rhs_float_size) => return parser::Type::Float(max(lhs_float_size, rhs_float_size)),
    //                 _ => {}
    //             }
    //         }
    //         _ => {}
    //     };
    //
    //     panic!("incompatible types.");
    // }

    fn allowed_implicit_conversion(&mut self, lhs: parser::Type, rhs: parser::Type) -> parser::Type {
        if variant_eq(&lhs, &rhs) {
            if self.get_size(lhs.clone()) > self.get_size(rhs.clone()) {
                return lhs;
            }

            return rhs;
        }

        const NUMBERS: [parser::Type; 11] = [
            parser::Type::I8,
            parser::Type::U8,
            parser::Type::I16,
            parser::Type::U16,
            parser::Type::I32,
            parser::Type::U32,
            parser::Type::I64,
            parser::Type::U64,
            parser::Type::F16,
            parser::Type::F32,
            parser::Type::F64,
        ];

        // the actual type here doesnt *really* matter, this function only serves to check if you
        // CAN do that operation, the casts will be calculated during IR gen
        if NUMBERS.contains(&lhs) && NUMBERS.contains(&rhs) {
            return parser::Type::F64;
        }

        panic!("unhandled case");
    }

    fn get_type(&mut self, expr: parser::Expression) -> parser::Type {
        match expr {
            parser::Expression::Null => parser::Type::Void,
            parser::Expression::Int(_) => parser::Type::I32,
            parser::Expression::Float(_) => parser::Type::F64,
            parser::Expression::String(_) => unimplemented!(),
            parser::Expression::Char(_) => parser::Type::Char,
            parser::Expression::Bool(_) => parser::Type::Bool,
            parser::Expression::Identifier(val) => {
                let s = self.get_symbol(val);
                return s.get_type();
            }
            parser::Expression::Unary { operator, member } => {
                match operator {
                    parser::Operator::BitwiseNot => parser::Type::I32,
                    parser::Operator::LogicalNot => parser::Type::Bool,
                    parser::Operator::Reference => parser::Type::I32,
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

                return self.allowed_implicit_conversion(lhs_type, rhs_type);
            }
            parser::Expression::Assignment { identifier, value } => {
                let lhs_type = self.get_type(*identifier);
                let rhs_type = self.get_type(*value);

                if lhs_type == rhs_type {
                    return lhs_type;
                }

                // TODO: type assignment should be stricter, assignment type should overwrite
                
                // match lhs_type {
                //     parser::Type::
                // }
                unimplemented!("assignement casts");
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
                        self.allowed_implicit_conversion(var_type, rhs_type);
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

    fn new_tmp(&mut self) -> String {
        self.t_count += 1;
        std::format!("t{}", self.t_count - 1)
    }

    fn tac_expr(&mut self, expr: parser::Expression) -> String {
        match expr {
            parser::Expression::Int(val) => val.to_string(),
            parser::Expression::Float(val) => val.to_string(),
            parser::Expression::Bool(val) => val.to_string(),
            parser::Expression::Identifier(val) => val,
            parser::Expression::Unary { operator, member } => {
                let nval = self.tac_expr(*member);
                let tmp = self.new_tmp();
                
                self.out.push_str(&std::format!("\t{} := {}{}\n", tmp, get_operator(operator), nval));
                return tmp;
            }
            parser::Expression::Binary { lhs, operator, rhs } => {
                let lval = self.tac_expr(*lhs);
                let rval = self.tac_expr(*rhs);
                
                let tmp = self.new_tmp();
                self.out.push_str(&std::format!("\t{} := {} {} {}\n", tmp, lval, get_operator(operator), rval));
                return tmp;
            }
            parser::Expression::FunctionCall { identifier, args } => {
                match *identifier {
                    parser::Expression::Identifier(val) => {
                        let tmp = self.new_tmp();
                        self.out.push_str(&std::format!("\t{} := CALL {}\n", tmp, val));
                        return tmp;
                    }
                    _ => self.tac_expr(*identifier)
                }
            }
            parser::Expression::Assignment { identifier, value } => {
                let rval = self.tac_expr(*value);
                let lhs = self.tac_expr(*identifier);
                self.out.push_str(&std::format!("\t{} := {}\n", lhs, rval));
                return "".to_owned();
            }
            parser::Expression::Null => unimplemented!(), 
            parser::Expression::Char(_) => unimplemented!(), 
            parser::Expression::String(_) => unimplemented!(), 
            parser::Expression::ArrayAccess {
                identifier,
                index,
            } => unimplemented!(), 
            // TODO: again, may remove the idea of this entirely and do it through desugaring  
            parser::Expression::ArrayConstructor { values } => unimplemented!(),
            parser::Expression::StructMember { identifier, val } => unimplemented!(),
            parser::Expression::StructConstructor { identifier, members } => unimplemented!(),
        }
    }

    fn tac_gen(&mut self, statement: parser::Statement) {
        match statement {
            parser::Statement::FunctionDeclaration { 
                name, 
                return_type: _, 
                parameters: _, 
                body, 
                public: _ 
            } => {
                let label = std::format!("L{}:\n", self.l_count);
                self.l_count += 1;
                self.out.push_str(&label);
                self.tac_gen(*body); 
            }
            parser::Statement::Block(statements) => {
                for s in statements {
                    self.tac_gen(*s);
                }
            }
            parser::Statement::VariableDeclaration { 
                identifier, 
                variable_type, 
                initial_value, 
                constant, 
                public, 
                global 
            } => {
                if let Some(s) = initial_value {
                    let tmp = self.tac_expr(*s);
                    self.out.push_str(&std::format!("\t{} := {}\n", identifier, tmp));
                } else {
                    self.out.push_str(&identifier);
                }
            }
            _ => {}
        }
    }

    fn get_globals(&mut self, ast: Vec<parser::Statement>) {
        self.add_table();
        for statement in ast {
            self.add_symbol(statement);
        }
    }

    fn semantic_analysis(&mut self, ast: Vec<parser::Statement>) {
        for statement in ast {
            self.analyze(statement, None);
        }
    }

    fn codegen(&mut self, ast: Vec<parser::Statement>) {
        for statement in ast {
            self.tac_gen(statement);
        }

        println!("{}", self.out);
    }

    pub fn generate(&mut self, ast: Vec<parser::Statement>) {
        self.get_globals(ast.clone());
        self.semantic_analysis(ast.clone());
        self.codegen(ast);
    }
}
