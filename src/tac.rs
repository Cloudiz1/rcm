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
    // fn get_size(&self) -> Option<usize>;
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
            // TODO: these two symbol tables need to be updated to check for ODR,
            // should be much simpler cause they only require a check on their current scope
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
    sizes: HashMap<String, usize>,
    temporary_count: u32, // _t0
    label_count: u32,     // _L0
    out: String,
}

impl TAC {
    pub fn new() -> Self {
        Self {
            tables: Vec::new(),
            sizes: HashMap::new(),
            temporary_count: 0,
            label_count: 0,
            out: String::new(),
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

    fn check_definition(&self, identifier: String) -> bool {
        let len = self.tables.len();
        for i in (0..len).rev() {
            if let Some(_) = self.tables[i].get(&identifier) {
                return true;
            }
        }

        return false;
    }

    // fn get_size(self, t: parser::Type) -> usize {
    //     match t {
    //         parser::Type::Pointer(_) => 8,
    //         parser::Type::Array { t, size: _} => self.get_size(*t),
    //         parser::Type::Void => 0,
    //         parser::Type::Bool => 1,
    //         parser::Type::I8 => 1,
    //         parser::Type::U8 => 1,
    //         parser::Type::I16 => 2,
    //         parser::Type::U16 => 2,
    //         parser::Type::I32 => 4,
    //         parser::Type::U32 => 4,
    //         parser::Type::I64 => 8,
    //         parser::Type::U64 => 8,
    //         parser::Type::F16 => 2,
    //         parser::Type::F32 => 4,
    //         parser::Type::F64 => 8,
    //         parser::Type::Char => 8,
    //         parser::Type::Typedef(val) => {
    //             let s = self.get_symbol(val);
    //         }
    //         _ => unimplemented!(),
    //     }
    // }

    fn get_symbol(&self, identifier: String) -> Symbol {
        // make sure its defined!
        if !self.check_definition(identifier.clone()) {
            panic!("unrecognized identifier");
        }

        let len = self.tables.len();
        for i in (0..len).rev() {
            if let Some(val) = self.tables[i].get(&identifier) {
                return (*val).clone();
            }
        }

        // should never be called, should be checked before calling this function
        panic!("symbol not found.");
    }

    fn add_symbol(&mut self, statement: parser::Statement) {
        let (name, symbol) = create_symbol_entry(statement);
        if self.check_definition(name.clone()) {
            panic!("identifier {} is already declared.", name);
        }

        self.get_current_table().insert(name, symbol);
    }

    fn handle_expression(&mut self, expr: parser::Expression) -> (parser::Type, String) {
        match expr {
            // parser::Expression::Null => parser::Type::Pointer(Box::new(parser::Type::Void)),
            parser::Expression::Int(val) => (parser::Type::I32, val.to_string()),
            parser::Expression::Float(val) => (parser::Type::F64, val.to_string()),
            // TODO: I doubt these are quite right
            parser::Expression::Char(val) => (parser::Type::Char, val.to_string()),
            parser::Expression::String(val) => (parser::Type::Str, val.to_string()),
            parser::Expression::Bool(val) => (parser::Type::Bool, val.to_string()),
            parser::Expression::Unary { operator, member } => {
                let (t, val) = self.handle_expression(*member);
                let out = match operator {
                    parser::Operator::Reference => {
                        let tmp = std::format!("t{}", self.temporary_count);
                        self.out.push_str(&std::format!("\t{} := &{}\n", tmp, val));
                        (parser::Type::Pointer(Box::new(t)), tmp)
                    }
                    parser::Operator::Negate => {
                        let tmp = std::format!("t{}", self.temporary_count);
                        self.out.push_str(&std::format!("\t{} := -{}\n", tmp, val));
                        (parser::Type::I32, tmp)
                    }
                    parser::Operator::BitwiseNot => {
                        let tmp = std::format!("t{}", self.temporary_count);
                        self.out.push_str(&std::format!("\t{} := ~{}\n", tmp, val));
                        (parser::Type::I32, tmp)
                    }
                    parser::Operator::LogicalNot => {
                        let tmp = std::format!("t{}", self.temporary_count);
                        self.out.push_str(&std::format!("\t{} := !{}\n", tmp, val));
                        (parser::Type::Bool, tmp)
                    }
                    parser::Operator::Dereference => {
                        let tmp = std::format!("t{}", self.temporary_count);
                        self.out.push_str(&std::format!("\t{} := *{}\n", tmp, val));
                        (parser::Type::Bool, tmp)
                    }
                    _ => unreachable!(),
                };

                self.temporary_count += 1;
                return out;
            }
            parser::Expression::Binary { lhs, operator, rhs } => {
                let (tlhs, lval) = self.handle_expression(*lhs);
                let (trhs, rval) = self.handle_expression(*rhs);

                let op = get_operator(operator.clone());
                let temp = std::format!("t{}", self.temporary_count);
                self.out.push_str(&std::format!("\t{} := {} {} {}\n", temp, lval, op, rval));
                self.temporary_count += 1;
                match operator {
                    parser::Operator::Add
                    | parser::Operator::Subtract
                    | parser::Operator::Multiply
                    | parser::Operator::Divide
                    | parser::Operator::Modulus => {
                        if variant_eq(&tlhs, &trhs) {
                            return (tlhs, temp);
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
                    | parser::Operator::BitwiseRightShift => (parser::Type::I32, temp),
                    parser::Operator::LogicalOr
                    | parser::Operator::LogicalAnd
                    | parser::Operator::Equal
                    | parser::Operator::NotEqual
                    | parser::Operator::LessThan
                    | parser::Operator::GreaterThan
                    | parser::Operator::LessThanEqual
                    | parser::Operator::GreaterThanEqual => (parser::Type::Bool, temp),
                    _ => panic!("invalid binary expression"),
                }
            }
            // TODO: like the type inference would be nice thanks
            parser::Expression::Identifier(val) => {
                (self.get_symbol(val.clone()).get_type().unwrap(), val)
            }
            // TODO: handle args
            parser::Expression::FunctionCall {
                identifier,
                args: _,
            } => {
                match *identifier {
                    parser::Expression::Identifier(val) => {
                        let s = self.get_symbol(val.clone());
                        let temp = std::format!("t{}", self.temporary_count);
                        self.out.push_str(&std::format!("\t{} := CALL {}\n", temp, val));
                        self.temporary_count += 1;

                        (
                            s.get_type().unwrap(),
                            temp
                        )
                    }
                    _ => self.handle_expression((*identifier).clone()),
                }
            }
            parser::Expression::ArrayAccess {
                identifier,
                index: _,
            } => self.handle_expression(*identifier),
            parser::Expression::Assignment { identifier, value } => {
                let (expected_type, lhs) = self.handle_expression(*identifier);
                let (rhs_type, rhs) = self.handle_expression(*value);
                if variant_eq(&expected_type, &rhs_type) {
                    return (expected_type, std::format!("{} := {}", lhs, rhs));
                }

                panic!("expected type on lhs differs from rhs.");
            }
            // parser::Expression::ArrayConstructor { mut values } => {
            //     let mut t1 = parser::Type::Void;
            //     if let Some(a) = values.pop() {
            //         t1 = self.handle_expression(*a);
            //     }
            //
            //     for val in values {
            //         let t2 = self.handle_expression(*val);
            //         if !variant_eq(&t1, &t2) {
            //             panic!("array can only have one data type.");
            //         }
            //     }
            //
            //     return t1;
            // }
            // parser::Expression::StructConstructor {
            //     identifier,
            //     members: _,
            // } => self.get_symbol(identifier).get_type().unwrap(),
            // // TODO: This shits like completely evil i think. I dont have a good way of checking if
            // // this is like part of a struct i dont think and currently nothing reads from a
            // // structs symbol table, so ill have to do some thinking
            // parser::Expression::StructMember { identifier, val } => unimplemented!(),
            _ => unimplemented!(),
        }
    }

    pub fn codegen(&mut self, statement: parser::Statement) {
        match statement {
            parser::Statement::Program(statements) => {
                self.add_table();
                for s in statements.clone() {
                    self.add_symbol(*s);
                    // add_symbol(&mut self.get_current_table(), *s);
                }

                for s in statements {
                    self.codegen(*s);
                }

                println!("{}", self.out);
            }
            parser::Statement::FunctionDeclaration {
                name,
                return_type: _,
                parameters: _,
                body,
                public: _,
            } => {
                self.out.push_str(&std::format!("{}:\n", name));
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
                    let (t, rhs) = self.handle_expression(*rhs);
                    self.out.push_str(&std::format!("\t{} := {}\n", identifier.clone(), rhs));
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

                self.add_symbol(s);
                // add_symbol(self.get_current_table(), s);
            }
            parser::Statement::ExpressionStatement(s) => {
                self.handle_expression(*s);
            }
            _ => {}
        };
    }
}

