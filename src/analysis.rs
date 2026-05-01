use crate::lexer;
use crate::parser;
use std::collections::HashMap;

fn variant_eq(a: &parser::Type, b: &parser::Type) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

#[derive(Debug, Clone)]
pub enum Symbol {
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
        names: Vec<String>,
        members: HashMap<String, Symbol>,
        methods: HashMap<String, Symbol>,
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
                typedef, ..
            } => parser::Type::Struct(typedef.clone()),
            Symbol::Member(t) => return t.clone(),
            // _ => {
            //     dbg!(self);
            //     panic!("can not get type");
            // }
            // Symbol::Enum {
            //     varients: _,
            //     public: _,
            //     typedef,
            // } => Some((*typedef).clone()),
        }
    }
}


fn contains_defintion(table: &HashMap<String, Symbol>, identifier: &String) -> bool {
    if let Some(_) = table.get(identifier) {
        return true;
    }

    return false;
}

fn check_definition(tables: &Vec<HashMap<String, Symbol>>, identifier: &String) -> bool {
    let len = tables.len();
    for i in (0..len).rev() {
        if contains_defintion(&tables[i], identifier) {
            return true;
        }
    }

    return false;
}

fn allowed_implicit_coercion(lhs: &parser::Type, rhs: &parser::Type) {
    if *lhs == *rhs {
        return
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

    if NUMBERS.contains(lhs) && NUMBERS.contains(rhs) {
        return;
    }

    panic!("can not convert from type {} to type {}", *lhs, *rhs);
}


pub struct Analyzer <'a>{
    tables: Vec<HashMap<String, Symbol>>,
    expression_arena: &'a Vec<parser::Expression>,
    types: Vec<parser::Type>,
    // sizes: HashMap<parser::Type, usize>,
}

// Types
impl<'a> Analyzer<'a> {
    pub fn new(expression_arena: &'a Vec<parser::Expression>) -> Self {
        // let mut sizes: HashMap<parser::Type, usize> = HashMap::new();
        // sizes.insert(parser::Type::I8, 1);
        // sizes.insert(parser::Type::U8, 1);
        // sizes.insert(parser::Type::I16, 2);
        // sizes.insert(parser::Type::U16, 2);
        // sizes.insert(parser::Type::I32, 4);
        // sizes.insert(parser::Type::U32, 4);
        // sizes.insert(parser::Type::I64, 8);
        // sizes.insert(parser::Type::U64, 8);
        // sizes.insert(parser::Type::F16, 2);
        // sizes.insert(parser::Type::F32, 4);
        // sizes.insert(parser::Type::F64, 8);
        // sizes.insert(parser::Type::Bool, 1);
        // sizes.insert(parser::Type::Char, 4);
        // sizes.insert(parser::Type::Pointer(), 4);

        Self {
            tables: Vec::new(),
            expression_arena,
            types: Vec::new(),
            // sizes
        } 
    }

    fn create_symbol_entry(&self, statement: &parser::Statement) -> (String, Symbol) {
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
                    let parser::Statement::Parameter { name: _, t } = *param.clone() else {
                        unreachable!();
                    };

                    params.push(t);
                }

                return (
                    name.clone(),
                    Symbol::Function { 
                        public: *public, 
                        return_type: return_type.clone(), 
                        params 
                    }
                );
            }
            parser::Statement::Parameter { name, t } => {
                return (
                    name.clone(),
                    Symbol::Variable { 
                        public: false, 
                        constant: false, 
                        variable_type: t.clone(), 
                    }
                )
            }
            parser::Statement::StructDeclaration { 
                name, 
                members, 
                methods, 
                public 
            } => {
                let mut names: Vec<String> = Vec::new();
                let mut symbol_members: HashMap<String, Symbol> = HashMap::new();
                for member in members {
                    let (member_name, symbol) = self.create_symbol_entry(&member);
                    if contains_defintion(&symbol_members, &member_name) {
                        panic!("cannot redefine member {} in struct {}", member_name, name);
                    }
                    
                    names.push(member_name.clone());
                    symbol_members.insert(member_name, symbol);
                }

                let mut symbol_methods: HashMap<String, Symbol> = HashMap::new();
                for method in methods {
                    let (method_name, symbol) = self.create_symbol_entry(&method);
                    if contains_defintion(&symbol_members, &method_name) {
                        panic!("cannot redefine method {} in struct {}", method_name, name);
                    }

                    symbol_methods.insert(method_name, symbol);
                }

                return (
                    name.clone(),
                    Symbol::Struct { 
                        public: *public, 
                        names,
                        members: symbol_members, 
                        methods: symbol_methods,
                        typedef: name.clone(),
                    }
                )
            }
            parser::Statement::Member { 
                name, 
                t, 
                public 
            } => {
                return (name.clone(), Symbol::Member(t.clone()));
            }
            parser::Statement::VariableDeclaration { 
                identifier, 
                variable_type, 
                initial_value, 
                constant, 
                public, 
                global 
            } => {
                let Some(t) = variable_type else {
                    unreachable!("all types should be known before calling create_symbol_entry()");
                };

                return (
                    identifier.clone(),
                    Symbol::Variable { 
                        public: *public, 
                        constant: *constant, 
                        variable_type: t.clone(),
                    }
                )
            }
            _ => panic!("can not create symbol from: {:?}", statement),
        }
    }

    fn condition_always_true(&self, condition: parser::ExpressionId) -> bool {
        // there may be other cases, but right now im really only looking at infinite loops
        match self.expression_arena[condition] {
            parser::Expression::Bool(val) => val == true,
            _ => false,
        }
    }

    /* always param is used for infinite loops, imagine:
    * ```
    * let i = 0;
    *  while true {
    *      // side effects...
    *      if i == 3 {
    *          return;
    *      }
    *  }
    *  ```
    *  the inner block won't always return, but there IS a return
    *  and infinite loops are allowed
    */
    // do NOT call this function if return type is void
    fn check_branch_return(&self, block: parser::Statement, always: bool) -> bool {
        match block {
            parser::Statement::Return { .. } => return true,
            parser::Statement::Block(statements) => {
                for statement in statements {
                    if self.check_branch_return(*statement, always) {
                        return true;
                    }
                }
                
                return false;
            }
            parser::Statement::IfStatement { 
                condition, 
                block, 
                alt 
            } => {
                if self.condition_always_true(condition) {
                    return self.check_branch_return(*block, always);
                }

                if let Some(else_block) = alt {
                    return self.check_branch_return(*block, always) && self.check_branch_return(*else_block, always);
                }

                if !always {
                    return self.check_branch_return(*block, always);
                }

                return false;
            }
            parser::Statement::WhileStatement { 
                condition, 
                block 
            } => {
                if self.condition_always_true(condition) {
                    // refer to comment above func decl
                    return self.check_branch_return(*block, false);
                }

                if !always {
                    return self.check_branch_return(*block, always);
                }

                return false;
            }
            _ => return false,
        }
    }

    fn get_symbol_dot(&mut self, expr: parser::ExpressionId) -> Symbol {
        let lhs = {
            let parser::Expression::Dot { lhs, .. } = &self.expression_arena[expr] else {
                unreachable!("Analyzer::get_symbol_dot(..) must be called with a dot node.");
            };

            *lhs
        };

        let lhs_type = self.get_type(lhs);
        let parser::Type::Struct(identifier) = lhs_type.clone() else {
            panic!("dot operator can not be applied to type {}", lhs_type);
        };

        let msg: &str = &std::format!("{} is not defined", &identifier);
        let lhs_symbol = self.get_symbol(&identifier, msg);

        let Symbol::Struct { 
            public,
            names,
            members,
            methods,
            typedef,
        } = lhs_symbol else {
            panic!("expected struct type, found type {}", lhs_type);
        };

        let parser::Expression::Dot { rhs, .. } = &self.expression_arena[expr] else {
            unreachable!();
        };

        if let Some(member) = members.get(rhs) {
            return member.clone();
        }

        if let Some(method) = methods.get(rhs) {
            return method.clone();
        };

        panic!("struct {} does not contain member {}", typedef, rhs);
    }

    fn get_symbol(&self, identifier: &String, msg: &str) -> Symbol {
        let len = self.tables.len();
        for i in 0..len {
            if let Some(symbol) = self.tables[i].get(identifier) {
                return symbol.clone();
            }
        }

        panic!("{}", msg);
    }

    fn get_type(&mut self, expr: parser::ExpressionId) -> parser::Type {
        let t = match self.expression_arena[expr].clone() {
            parser::Expression::Null => parser::Type::Void,
            parser::Expression::Int(_) => parser::Type::I32,
            parser::Expression::Float(_) => parser::Type::F64,
            parser::Expression::String(_) => unimplemented!(),
            parser::Expression::Char(_) => parser::Type::Char,
            parser::Expression::Bool(_) => parser::Type::Bool,
            parser::Expression::Identifier(val) => {
                return self.get_symbol(&val, "unrecognized identifier").get_type();
            }
            parser::Expression::Unary { operator, member } => {
                match operator {
                    lexer::Token::Tilde => parser::Type::I32,
                    lexer::Token::Bang => parser::Type::Bool,
                    lexer::Token::Ampersand => {
                        let inner_type = self.get_type(member);
                        return parser::Type::Pointer(Box::new(inner_type));
                    }
                    lexer::Token::DotStar => {
                        let parser::Type::Pointer(nested) = self.get_type(member) else { unreachable!() };
                        return *nested;
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
                    return parser::Type::Bool;
                }
                
                if lhs_type != rhs_type {
                    panic!("expected type {} found type {}", lhs_type, rhs_type);
                }
                
                return lhs_type;
            }
            parser::Expression::Dot { .. } => {
                return self.get_symbol_dot(expr).get_type();
            }
            parser::Expression::Assignment { identifier, value } => {
                let lhs_type = self.get_type(identifier);
                let rhs_type = self.get_type(value);

                if lhs_type == rhs_type {
                    return lhs_type;
                }

                panic!("can not assign type {} to type {}", rhs_type, lhs_type);
            }
            parser::Expression::FunctionCall { 
                identifier, 
                args 
            } => {
                let symbol = match &self.expression_arena[identifier] {
                    parser::Expression::Identifier(name) => {
                        let msg = &std::format!("unrecognized function {}", name);
                        self.get_symbol(&name, msg)
                    }
                    parser::Expression::Dot { .. } => {
                        self.get_symbol_dot(expr)
                    }
                    _ => unimplemented!("function pointers do not exist"),
                };

                let Symbol::Function { 
                    return_type, 
                    params,
                    ..
                } = symbol else {
                    panic!("expected function");
                };

                for (arg, expected) in std::iter::zip(args.clone(), params) {
                    let arg_type = self.get_type(arg);
                    if arg_type != expected {
                        panic!("expected argument of type {}, found type {}", expected, arg_type);
                    }
                }

                return return_type;
            }
            parser::Expression::ArrayAccess { 
                identifier, 
                index: _ 
            } => {
                let parser::Type::Array { 
                    t, 
                    size: _ 
                } = self.get_type(identifier) else {
                    panic!("type {} can not be indexed", self.get_type(identifier));
                };

                return *t;
            }
            parser::Expression::ArrayConstructor { values } => {
                let expected_type = self.get_type(values[0]);
                let len = values.len();
                for value in values {
                    if !variant_eq(&expected_type, &self.get_type(value)) {
                        panic!("all expressions in array constructor must have the same type.");
                    }
                }

                return parser::Type::Array { 
                    t: Box::new(expected_type), 
                    size: Some(len) 
                };
            }
            parser::Expression::StructConstructor { 
                identifier, 
                members 
            } => {
                let symbol = self.get_symbol(&identifier, "unrecognized struct");
                let Symbol::Struct { 
                    public, 
                    names,
                    members: defined_members, 
                    methods,
                    typedef 
                } = symbol.clone() else {
                    panic!("expected struct"); 
                };

                // if the lengths are different, see which one(s) are missing
                if members.len() != defined_members.len() {
                    let constructed_members = members
                        .keys()
                        .map(|x| x.clone())
                        .collect::<Vec<String>>();

                    for member in defined_members.keys() {
                        if !constructed_members.contains(member) {
                            // TODO: collect all the missing ones and make one big error
                            panic!("missing member constructor: {}", member);
                        }
                    }
                }

                for (member_name, member_val) in members {
                    let Some(defined_member_symbol) = defined_members.get(&member_name) else {
                        panic!("unrecognized struct member");
                    };

                    let expected_type = defined_member_symbol.get_type();
                    let found_type = self.get_type(member_val);
                    if expected_type != found_type {
                        panic!("expected type {}, found type {}", expected_type, found_type);
                    }
                } 

                return symbol.get_type();
            }
        };

        self.types.insert(expr, t.clone());
        return t;
    }
}

impl<'a> Analyzer<'a> {
    fn analyze_statement(&mut self, statement: &parser::Statement, function_return: &Option<parser::Type>) {
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
                    self.add_symbol(param);
                }

                match return_type {
                    parser::Type::Void => {}
                    _ => {
                        if !self.check_branch_return(*body.clone(), true) {
                            panic!("expected return of type {}, found no return", return_type);
                        }
                    }
                };

                self.analyze_statement(body, &Some(return_type.clone()));
                self.remove_table();
            }
            parser::Statement::Return { value } => {
                let return_type = match value { // get type on rhs
                    None => parser::Type::Void,
                    Some(v) => self.get_type(*v),
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
                let condition_type = self.get_type(*condition);
                // TODO:
                // if foo { .. } (maybe just `if foo { .. }` -> `if foo == null { .. }`?)
                if !variant_eq(&condition_type, &parser::Type::Bool) {
                    panic!("condition must contain bool expression");
                }

                self.analyze_statement(block, function_return);
                if let Some(else_block) = alt {
                    self.analyze_statement(else_block, function_return);
                }
            }
            parser::Statement::Block(statements) => {
                self.add_table();
                for statement in statements {
                    self.analyze_statement(statement, function_return);
                }
                self.remove_table();
            }
            parser::Statement::VariableDeclaration { 
                identifier, 
                variable_type, 
                initial_value, 
                constant, 
                public, 
                global 
            } => {
                let mut new_var_type = variable_type.clone();
                if let Some(rhs) = initial_value.clone() {
                    let rhs_type = self.get_type(rhs);
                    if let Some(ref var_type) = new_var_type {
                        allowed_implicit_coercion(var_type, &rhs_type);
                    } else {
                        new_var_type = Some(rhs_type);
                    }
                } else {
                    let Some(_) = new_var_type else {
                        panic!("type of variable {} must be known at compile time.", identifier);
                    };
                }

                let var = parser::Statement::VariableDeclaration { 
                    identifier: identifier.clone(), 
                    variable_type: new_var_type, 
                    initial_value: *initial_value, 
                    constant: *constant, 
                    public: *public,
                    global: *global,
                };

                self.add_symbol(&var);
            }
            parser::Statement::ExpressionStatement(expr) => {
                self.get_type(*expr);
            }
            _ => (),
        }
    }
}

impl<'a> Analyzer<'a> {
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

    fn add_symbol(&mut self, statement: &parser::Statement) {
        let (name, symbol) = self.create_symbol_entry(statement);
        if check_definition(&self.tables, &name) {
            panic!("cannot redefine identifier {}", name);
        }

        self.get_current_table().insert(name, symbol);
    }

    fn get_globals(&mut self, ast: &Vec<parser::Statement>) {
        self.add_table();
        for statement in ast {
            match statement {
                // i dont actually have a good way of differentiating globals from not, so to
                // prevent erroneous ODR violations in the second scan, we ignore globals here
                parser::Statement::VariableDeclaration { .. } => continue,
                _ => self.add_symbol(statement)
            }
        }
    }

    pub fn analyze(&mut self, ast: &Vec<parser::Statement>) -> HashMap<String, Symbol> {
        self.get_globals(ast);

        for statement in ast {
            self.analyze_statement(statement, &None);
        }

        return std::mem::take(&mut self.tables[0]);
    }
}
