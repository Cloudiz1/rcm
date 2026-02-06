use crate::lexer;
use crate::parser;
use std::collections::HashMap;

// TODO: make this a generic...
fn variant_eq(a: &parser::Type, b: &parser::Type) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

#[derive(Debug, Clone)]
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
        // methods: HashMap<String, Symbol>,
        typedef: String,
    },
    Member(parser::Type),
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
                return variable_type.clone();
            }
            Symbol::Function {
                params: _,
                return_type,
                public: _,
            } => return_type.clone(),
            Symbol::Struct {
                members: _,
                // methods: _,
                public: _,
                typedef,
            } => parser::Type::Struct(typedef.clone()),
            _ => panic!("can not get type."),
            // Symbol::Enum {
            //     varients: _,
            //     public: _,
            //     typedef,
            // } => Some((*typedef).clone()),
        }
    }
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
                symbol_members.insert(name, symbol);
            }

            for method in methods {
                let (name, symbol) = create_symbol_entry(*method);
                check_definition_single(&symbol_members, &name);
                symbol_members.insert(name, symbol);
            }

            return (
                name.clone(),
                Symbol::Struct { 
                    public, 
                    members: symbol_members, 
                    // methods: symbol_methods,
                    typedef: name,
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

fn allowed_implicit_coercion(lhs: parser::Type, rhs: parser::Type) {
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

    if NUMBERS.contains(&lhs) && NUMBERS.contains(&rhs) {
        return;
    }

    panic!("can not convert from type {} to type {}", lhs, rhs);
}

fn does_branch_return(statement: parser::Statement) -> bool {
    match statement {
        parser::Statement::Block(statements) => {
            for s in statements {
                if let parser::Statement::Return { .. } = *s {
                    return true;
                };
            }

            return false;
        }
        _ => unreachable!("Analyzer::does_branch_return(..) should only be called with statement parser::Statement::Block")
    }
}

// do NOT call this function if return type is void
fn does_block_always_return(block: parser::Statement) -> bool {
    let parser::Statement::Block(statements) = block else {
        unreachable!("Analyzer::does_block_return(..) should only be called with statement parser::Statement::Block")
    };

    for statement in statements {
        // TODO: if its another block, check if that one always returns!
        
        // if variant_eq(statement.clone(), &parser::Statement::Block(Vec::new())) {
        //     if does_block_always_return(statement.clone()) {
        //         return true;
        //     }
        // }
        match *statement {
            parser::Statement::Return { .. } => return true,
            parser::Statement::IfStatement { 
                condition, 
                block, 
                alt 
            } => {
                // TODO:
                // if condition is always true, just check if block returns
                // if there is an else block, just check if both return (done)
                // otherwise, return false (done)
                if let Some(else_block) = alt {
                    return does_block_always_return(*block) && does_block_always_return(*else_block);
                }

                return false;
            }
            parser::Statement::WhileStatement { 
                condition, 
                block 
            } => {
                // TODO:
                // if its always true, just check if it returns!
                // otherwise, return false
            }
            _ => {}
        }
    }

    return false;
}

pub struct Analyzer {
    tables: Vec<HashMap<String, Symbol>>,
}

// Types
impl Analyzer {
    fn get_symbol(&mut self, identifier: &String) -> Option<Symbol> {
        let len = self.tables.len();
        for i in 0..len {
            if let Some(symbol) = self.tables[i].get(identifier) {
                return Some(symbol.clone());
            }
        }

        return None;
    }

    fn get_type(&mut self, expr: &parser::Expression) -> parser::Type {
        match expr {
            parser::Expression::Null => parser::Type::Void,
            parser::Expression::Int(_) => parser::Type::I32,
            parser::Expression::Float(_) => parser::Type::F64,
            parser::Expression::String(_) => unimplemented!(),
            parser::Expression::Char(_) => parser::Type::Char,
            parser::Expression::Bool(_) => parser::Type::Bool,
            parser::Expression::Identifier(val) => {
                if let Some(symbol) = self.get_symbol(&val) {
                    return symbol.get_type();
                }

                panic!("unrecognized identifier");
            }
            parser::Expression::Unary { operator, member } => {
                match operator {
                    lexer::Token::Tilde => parser::Type::I32,
                    lexer::Token::Bang => parser::Type::Bool,
                    lexer::Token::Ampersand => parser::Type::I32,
                    lexer::Token::DotStar => {
                        self.get_type(member)
                    }
                    _ => panic!("unexpected operator in unary expression. found: {:?}", operator),
                }
            }
            parser::Expression::Binary { lhs, operator, rhs } => {
                let lhs_type = self.get_type(lhs); 
                let rhs_type = self.get_type(rhs); 

                let boolean_expr = match operator {
                    lexer::Token::DoublePipe
                    | lexer::Token::DoubleAmpersand
                    | lexer::Token::EqualEqual
                    | lexer::Token::BangEqual
                    | lexer::Token::LeftCaret
                    | lexer::Token::RightCaret
                    | lexer::Token::LeftCaretEqual
                    | lexer::Token::RightCaretEqual => true,
                    _ => false,
                };

                if boolean_expr {
                    // TODO: all equality operations, however, must be between ints
                    // TODO: maybe let null be an int?
                    return parser::Type::Bool;
                }
                
                if lhs_type != rhs_type {
                    panic!("expected type {} found type {}", lhs_type, rhs_type);
                }
                
                return lhs_type;
            }
            parser::Expression::Dot { lhs, rhs } => {
                let lhs_type = self.get_type(lhs);
                let parser::Type::Struct(identifier) = lhs_type.clone() else {
                    panic!("dot operator can not be applied to type {}", lhs_type);
                };

                let Some(lhs_symbol) = self.get_symbol(&identifier) else {
                    panic!("{} is not defined", identifier);
                };

                let Symbol::Struct { 
                    members, 
                    typedef,
                    ..
                } = lhs_symbol else {
                    panic!("expected struct type, found type {}", lhs_type);
                };

                let Some(member) = members.get(rhs) else {
                    panic!("struct {} does not contain member {}", typedef, rhs);
                };

                return member.get_type();
            }
            parser::Expression::Assignment { identifier, value } => {
                let lhs_type = self.get_type(identifier);
                let rhs_type = self.get_type(value);

                if lhs_type == rhs_type {
                    return lhs_type;
                }

                // TODO: type assignment should be stricter, assignment type should overwrite
                // i feel like i can do this in codegen actually
                
                // match lhs_type {
                //     parser::Type::
                // }
                unimplemented!("assignement casts");
            }
            parser::Expression::FunctionCall { identifier, args: _ } => self.get_type(identifier),
            parser::Expression::ArrayAccess { identifier, index: _ } => self.get_type(identifier),
            parser::Expression::ArrayConstructor { values } => {
                let expected_type = self.get_type(&values[0]);
                for value in values {
                    if !variant_eq(&expected_type, &self.get_type(value)) {
                        panic!("all expressions in array constructor must have the same type.");
                    }
                }

                return expected_type;
            }
            parser::Expression::StructMember { 
                parent, 
                identifier, 
                val 
            } => {
                return parser::Type::Void;
            }
            // TODO: lookup
            // parser::Expression::StructMember { 
            //     parent,
            //     identifier, 
            //     val 
            // } => {
            //     let parent_struct = self.get_symbol(parent);
            //     match parent_struct {
            //         Symbol::Struct { 
            //             members, 
            //             public 
            //         } => {
            //             let Some(child) = members.get(&identifier) else {
            //                 panic!();
            //             };
            //
            //             return child.get_type()
            //         }
            //         _ => panic!(),
            //     }
            // }
            parser::Expression::StructConstructor { identifier, members } => {
                todo!();
            }
        }
    }
}

impl Analyzer {
    fn analyze_statement(&mut self, statement: parser::Statement, function_return: &Option<parser::Type>) {
        match statement {
            parser::Statement::FunctionDeclaration { 
                name, 
                return_type, 
                parameters, 
                body, 
                public 
            } => {
                self.add_table();
                for param in parameters {
                    self.add_symbol(*param);
                }

                self.analyze_statement(*body, &Some(return_type));
                self.remove_table();
            }
            parser::Statement::Return { value } => {
                let return_type = match value { // get type on rhs
                    None => parser::Type::Void,
                    Some(v) => self.get_type(&v),
                };
                
                let Some(expected_return) = function_return else {
                    panic!("unexpected return");
                };

                if !variant_eq(&return_type, expected_return) {
                    panic!("expected return of type {}, found type {}", expected_return, return_type); 
                }
            } 
            parser::Statement::IfStatement { 
                condition, 
                block, 
                alt 
            } => {
                let condition_type = self.get_type(&condition);
                // TODO:
                // if foo { .. } (maybe just `if foo { .. }` -> `if foo == null { .. }`?)
                if !variant_eq(&condition_type, &parser::Type::Bool) {
                    panic!("condition must contain bool expression");
                }

                
                self.analyze_statement(*block, function_return);
                if let Some(else_block) = alt {
                    self.analyze_statement(*else_block, function_return);
                }
            }
            parser::Statement::Block(statements) => {
                self.add_table();
                for statement in statements {
                    self.analyze_statement(*statement, function_return);
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
                    let rhs_type = self.get_type(&rhs);
                    if let Some(var_type) = variable_type.clone() {
                        allowed_implicit_coercion(var_type, rhs_type);
                    } else {
                        variable_type = Some(rhs_type);
                    }
                } else {
                    let Some(_) = variable_type else {
                        panic!("type of variable {} must be known at compile time.", identifier);
                    };
                }

                let var = parser::Statement::VariableDeclaration { 
                    identifier, 
                    variable_type, 
                    initial_value, 
                    constant, 
                    public, 
                    global 
                };

                self.add_symbol(var);
            }
            parser::Statement::ExpressionStatement(expr) => {
                self.get_type(&expr);
            }
            _ => {}
        }
    }
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
        if check_definition(&self.tables, &name) {
            panic!("cannot redefine identifier {}", name);
        }

        self.get_current_table().insert(name, symbol);
    }

    fn get_globals(&mut self, ast: Vec<parser::Statement>) {
        self.add_table();
        for statement in ast {
            self.add_symbol(statement);
            // add_symbol(&mut self.tables, statement);
        }

        // dbg!(&self.tables);
    }

    pub fn analyze(&mut self, ast: Vec<parser::Statement>) {
        self.get_globals(ast.clone());

        for statement in ast {
            self.analyze_statement(statement, &None);
        }
    }
}
