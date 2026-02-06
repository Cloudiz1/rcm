use crate::lexer;
use crate::util;
use std::collections::HashMap;
use std::vec::Vec;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Pointer(Box<Type>),
    Array {
        t: Box<Type>,
        size: Option<usize>, // this will always be known when passed to TAC
    },
    Struct(String),
    // Struct {
    //     members: HashMap<String, Type>,
    //     methods: HashMap<String, Type>,
    // },
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    F16,
    F32,
    F64,
    Bool,
    Void,
    Str,
    Char,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Pointer(nested_type) => {
                write!(f, "*{}", *nested_type)
            }
            Type::Array { t, size: _ } => {
                write!(f, "[{}]", *t)
            }
            Type::Struct(t) => write!(f, "{}", t),
            Type::I8 => write!(f, "i8"),
            Type::U8 => write!(f, "u8"),
            Type::I16 => write!(f, "i16"),
            Type::U16 => write!(f, "u16"),
            Type::I32 => write!(f, "i32"),
            Type::U32 => write!(f, "u32"),
            Type::I64 => write!(f, "i64"),
            Type::U64 => write!(f, "u64"),
            Type::F16 => write!(f, "f16"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::Str => write!(f, "string"),
            Type::Char => write!(f, "char"),
        }
    }
}

pub struct Parser {
    input: Vec<lexer::DebugToken>,
    i: usize,
    types: HashMap<String, Type>,
    panic: bool,
    // out: Vec<Statement>,
    lines: Vec<String>,
    src_path: String,
    errored: bool,
}

#[derive(Debug, Clone)]
pub enum Expression {
    // expressions
    Null,
    Int(i64),
    Float(f64),
    String(String),
    Identifier(String),
    Char(char),
    Bool(bool),
    Unary {
        operator: lexer::Token,
        member: Box<Expression>,
    },
    Binary {
        lhs: Box<Expression>,
        operator: lexer::Token,
        rhs: Box<Expression>,
    },
    Dot {
        lhs: Box<Expression>, // these have to decay to identifiers
        rhs: String, // because its left associative, rhs will always be a string!
    },
    Assignment {
        identifier: Box<Expression>,
        value: Box<Expression>,
    },
    FunctionCall {
        identifier: Box<Expression>, // in case i want function pointers (foo())()
        args: Vec<Box<Expression>>,
    },
    ArrayAccess {
        identifier: Box<Expression>,
        index: Box<Expression>,
    },
    ArrayConstructor {
        values: Vec<Box<Expression>>,
    },
    StructMember {
        parent: String,
        identifier: String,
        val: Box<Expression>,
    },
    StructConstructor {
        identifier: String,
        members: Vec<Box<Expression>>, // struct members
    },
}

#[derive(Debug, Clone)]
pub enum Statement {
    ParseError, // used for panic mode
    ExpressionStatement(Expression),
    Block(Vec<Box<Statement>>),
    VariableDeclaration {
        identifier: String,
        variable_type: Option<Type>,
        initial_value: Option<Box<Expression>>,
        constant: bool,
        public: bool,
        global: bool,
    },
    // EnumDeclaration {
    //     name: String,
    //     varients: Vec<String>,
    //     public: bool,
    // },
    Parameter {
        name: String,
        t: Type,
    },
    Return {
        value: Option<Box<Expression>>,
    },
    FunctionDeclaration {
        name: String,
        return_type: Type,
        parameters: Vec<Box<Statement>>,
        body: Box<Statement>,
        public: bool,
    },
    Member {
        name: String,
        t: Type,
        public: bool,
    },
    StructDeclaration {
        name: String,
        members: Vec<Box<Statement>>, // vec of members
        methods: Vec<Box<Statement>>, // vec of function decs.
        public: bool,
    },
    IfStatement {
        condition: Box<Expression>,
        block: Box<Statement>,
        alt: Option<Box<Statement>>,
    },
    WhileStatement {
        condition: Box<Expression>,
        block: Box<Statement>,
    },
}

// helpers
impl Parser {
    pub fn new(input: Vec<lexer::DebugToken>, lines: Vec<String>, src_path: String) -> Self {
        let mut types: HashMap<String, Type> = HashMap::new();
        types.insert("i8".to_owned(), Type::I8);
        types.insert("u8".to_owned(), Type::U8);
        types.insert("i16".to_owned(), Type::I16);
        types.insert("u16".to_owned(), Type::U16);
        types.insert("i32".to_owned(), Type::I32);
        types.insert("u32".to_owned(), Type::U32);
        types.insert("i64".to_owned(), Type::I64);
        types.insert("u64".to_owned(), Type::U64);
        types.insert("f16".to_owned(), Type::F16);
        types.insert("f32".to_owned(), Type::F32);
        types.insert("f64".to_owned(), Type::F64);
        // types.insert("isize".to_owned(), Type::Isize);
        // types.insert("usize".to_owned(), Type::Usize);
        types.insert("bool".to_owned(), Type::Bool);
        types.insert("void".to_owned(), Type::Void);
        types.insert("string".to_owned(), Type::Str);
        types.insert("char".to_owned(), Type::Char);

        Self {
            input,
            i: 0,
            types,
            panic: false,
            // out: Vec::new(),
            lines,
            src_path,
            errored: false,
        }
    }

    fn advance(&mut self) {
        self.i += 1;
    }

    fn at_end(&self) -> bool {
        self.i >= self.input.len()
    }

    fn previous(&self) -> lexer::Token {
        if self.i == 0 {
            panic!("Parser.previous() called on index 0.");
        }

        self.input[self.i - 1].token_type.clone()
    }

    fn current(&self) -> lexer::Token {
        if self.at_end() {
            panic!("Parser.current() tried to access out of bounds index.");
        }

        self.input[self.i].token_type.clone()
    }

    // can peek backwards too!
    fn peek(&self, index: i32) -> Option<lexer::Token> {
        let i = self.i as i32 + index - 1;
        
        if i >= self.input.len() as i32 || i < 0 {
            return None;
        }

        Some(self.input[i as usize].token_type.clone())
    }

    fn consume(&mut self) -> lexer::Token {
        self.i += 1;
        self.previous()
    }

    fn get_debug_token(&self) -> lexer::DebugToken {
        if self.i == 0 {
            return self.input[0].clone();
        }

        self.input[self.i - 1].clone()
    }

    fn token_match(&self, tokens: &[lexer::Token]) -> bool {
        tokens.contains(&self.current())
    }

    // matches current and advances if true
    fn match_advance(&mut self, tokens: &[lexer::Token]) -> bool {
        if self.token_match(tokens) {
            self.i += 1;
            return true;
        }

        return false;
    }

    fn create_binary(&self, lhs: Expression, operator: lexer::Token, rhs: Expression) -> Expression {
        Expression::Binary {
            lhs: Box::new(lhs),
            operator,
            rhs: Box::new(rhs),
        }
    }

    fn create_unary(&self, operator: lexer::Token, member: Expression) -> Expression {
        Expression::Unary {
            operator,
            member: Box::new(member),
        }
    }

    /*
     * consumes token
     */
    fn expect(&mut self, token: lexer::Token, msg: &str) -> bool {
        if !self.at_end() && self.current() == token {
            self.advance();
            return true;
        }

        self.error(msg);
        return false;
    }

    fn unwrap_identifier(&mut self, i: Expression, msg: &str) -> String {
        if let Expression::Identifier(identifier) = i {
            return identifier;
        };

        self.error(msg);
        return "".to_owned();
    }

    fn is_public(&mut self) -> bool {
        if self.i == 0 {
            return false;
        }

        self.previous() == lexer::Token::Pub
    }

    fn parse_array_expr(&mut self, expr: Expression) -> i64 {
        // dbg!(&expr);
        match expr {
            Expression::Int(val) => return val,
            Expression::Binary { lhs, operator, rhs } => {
                let elhs = self.parse_array_expr(*lhs);
                let erhs = self.parse_array_expr(*rhs);

                match operator {
                    lexer::Token::Plus => elhs + erhs,
                    lexer::Token::Minus => elhs - erhs,
                    lexer::Token::Star => elhs * erhs,
                    lexer::Token::Slash => elhs / erhs,
                    lexer::Token::Percent => elhs % erhs,
                    _ => {
                        self.error("Unallowed expression.");
                        return 0;
                    }
                }
            }
            _ => {
                self.error("Unallowed expression.");
                return 0;
            }
        }
    }

    // TODO: refactor this too
    fn parse_type(&mut self) -> Type {
        match self.current() {
            lexer::Token::LBrace => {
                self.advance();
                let mut size: Option<usize> = None;
                if !self.match_advance(&[lexer::Token::Underscore]) {
                    let expr = self.term();
                    let inferred_size = self.parse_array_expr(expr);
                    if inferred_size < 1 {
                        self.error("array size must be larger than 0");
                    }

                    size = Some(inferred_size as usize);
                }

                if !self.expect(lexer::Token::RBrace, "expected closing brace.") {
                    return Type::Void;
                }

                Type::Array {
                    t: Box::new(self.parse_type()),
                    size,
                }
            } 
            lexer::Token::Star => {
                self.advance(); // consumes the star
                Type::Pointer(Box::new(self.parse_type()))
            }
            lexer::Token::Identifier(identifier) => {
                self.advance();
                if let Some(t) = self.types.get(&identifier) {
                    return t.clone();
                } else {
                    return Type::Struct(identifier);
                }
            }
            _ => {
                self.error("expected type");
                Type::Void
            }
        }
    }

    // fn print(&self) {
    //     println!("{:?}", self.current());
    // }
}

impl Parser {
    fn error(&mut self, msg: &str) {
        if self.panic {
            return;
        }

        let token = self.get_debug_token();

        // dbg!(token.token_type, token.column + 1);
        if !self.panic {
            util::print_error(token, &self.lines, &self.src_path, msg);
        }

        self.panic = true;
        self.errored = true;
    }

    fn synchronize(&mut self) {
        while !self.at_end() {
            match self.current() {
                lexer::Token::Semicolon => {
                    self.advance();
                    return;
                }

                lexer::Token::Pub
                | lexer::Token::Fn
                | lexer::Token::Struct
                | lexer::Token::Enum
                | lexer::Token::If
                | lexer::Token::Else
                | lexer::Token::For
                | lexer::Token::While
                | lexer::Token::Return => {
                // | lexer::Token::RCurly => {
                    return;
                }
                _ => {}
            }

            self.advance();
        }
    }
}

// statements
impl Parser {
    // i know the vec is silly but its for struct method desugaring
    fn declaration(&mut self) -> Statement {
        if self.current() == lexer::Token::Pub {
            self.advance();
        }

        let out = match self.current() {
            lexer::Token::Fn => self.function_declaration(),
            lexer::Token::Struct => self.struct_declaration(),
            // lexer::Token::Enum => self.enum_declaration(),
            lexer::Token::Let | lexer::Token::Const => self.variable_declaration(true),
            _ => {
                self.error("unrecognized top level expression");
                Statement::ParseError
                // vec![Statement::ParseError]
            }
        };

        if self.panic {
            self.synchronize();
            self.panic = false;
        }

        return out;
    }

    // fn function_declaration(&mut self, struct_name: Option<String>) -> Statement {
    fn function_declaration(&mut self) -> Statement {
        let public = self.is_public();

        self.advance(); // consume fn token
        let mut identifier = self.primary();
        let name = self.unwrap_identifier(identifier, "expected function name after keyword 'fn'");

        self.expect(lexer::Token::LParen, "expected '(' after function name.");

        let mut parameters: Vec<Box<Statement>> = Vec::new();
        while self.current() != lexer::Token::RParen {
            identifier = self.primary();
            let param_name = self.unwrap_identifier(
                identifier,
                "expected parameter name in function declaration.",
            );

            self.expect(lexer::Token::Colon, "expected colon after parameter name.");

            let param_type = self.parse_type();
            parameters.push(Box::new(Statement::Parameter {
                name: param_name,
                t: param_type,
            }));

            if self.current() == lexer::Token::Comma {
                self.advance();
                continue;
            }

            if let lexer::Token::Identifier(_) = self.current() {
                self.error("missing comma between function parameters.");
            }
        }

        self.expect(lexer::Token::RParen, "expected ')'");

        let return_type = self.parse_type();

        // if let Some(n) = struct_name {
        //     name = std::format!("{}@{}", n, name);
        // }

        let body = Box::new(self.block());
        Statement::FunctionDeclaration {
            name,
            return_type,
            parameters,
            body,
            public,
        }
    }

    // Currently allows zero-sized structs, not sure if i want this or not
    fn struct_declaration(&mut self) -> Statement {
        let struct_public = self.is_public();
        self.advance(); // consume struct token

        let identifier = self.primary();
        let struct_name = self.unwrap_identifier(identifier, "expected struct name after struct keyword.");

        if !self.expect(lexer::Token::LCurly, "expected body of struct.") {
            return Statement::ParseError;
            // return vec![Statement::ParseError];
        }

        let mut members: Vec<Box<Statement>> = Vec::new();
        let mut comma: bool = true;
        loop {
            if self.current() == lexer::Token::Pub {
                self.advance();
                continue;
            }

            let lexer::Token::Identifier(identifier) = self.current() else {
                break
            };

            if !comma {
                self.error("expected comma between member declarations");
            }

            let public: bool = self.is_public();
            self.advance(); // consume the identifier
            self.expect(lexer::Token::Colon,"expected colon folllowing member declaration");

            let t = self.parse_type();

            comma = false;
            if self.current() == lexer::Token::Comma {
                comma = true;
                self.advance();
            }

            members.push(Box::new(Statement::Member {
                name: identifier,
                t,
                public,
            }));
        }

        let mut methods: Vec<Box<Statement>> = Vec::new();
        loop {
            if self.current() == lexer::Token::Pub {
                self.advance();
                continue;
            }

            if self.current() == lexer::Token::Fn {
                methods.push(Box::new(self.function_declaration()));
            } else {
                break;
            }
        }

        match self.current() {
            lexer::Token::Pub | lexer::Token::Identifier(_) => {
                self.error("member declarations must go before methods.");
            }
            _ => {}
        }

        self.expect(lexer::Token::RCurly, "expected closing curly on struct declaration.");

        return Statement::StructDeclaration {
            name: struct_name,
            members,
            methods,
            public: struct_public,
        };
    }

    // fn enum_declaration(&mut self) -> Statement {
    //     let public = self.is_public();
    //     self.advance(); // consume enum token
    //     let mut identifier = self.primary();
    //     let name = self.unwrap_identifier(identifier, "expected identifier in enum declaration.");
    //
    //     let mut varients: Vec<String> = Vec::new();
    //
    //     if !self.expect(lexer::Token::LCurly, "expected body in enum declaration.") {
    //         return Statement::ParseError;
    //     }
    //
    //     while self.current() != lexer::Token::RCurly {
    //         identifier = self.primary();
    //         varients.push(self.unwrap_identifier(identifier, "expected varient in enum body."));
    //
    //         let token = self.current();
    //         if token != lexer::Token::Comma && token != lexer::Token::RCurly {
    //             self.error("expected comma seperating enum varients.");
    //         }
    //
    //         if token == lexer::Token::Comma {
    //             self.advance();
    //         }
    //     }
    //
    //     self.expect(
    //         lexer::Token::RCurly,
    //         "expected closing right curly after enum declaration.",
    //     );
    //
    //     Statement::EnumDeclaration {
    //         name,
    //         varients,
    //         public,
    //     }
    // }
 
    // TODO: The Great Refactoring (TM) on this
    fn get_implicit_array_length(&mut self, array_type: Type, rhs: Option<Box<Expression>>) -> Type {
        match array_type {
            Type::Array { t: var_t, size } => {
                // If we know the size, no guessing needed, return
                match size {
                    Some(_) => return Type::Array {
                        t: var_t,
                        size
                    },
                    None => {}
                }

                let nested_array_option = match *var_t.clone() {
                    // if there is another array, call recursively until there isnt
                    Type::Array { t: nested_var_t, size: nested_size} => {
                        // we know the size, no guessing needed
                        if let Some(s) = nested_size {
                            Some(Type::Array { t: nested_var_t, size: Some(s)})
                        } else {
                            let Some(expr) = rhs.clone() else {
                                self.error("can not infer array length.");
                                return Type::Void;
                            };

                            // recursive only if you find another array constructor
                            match *expr {
                                Expression::ArrayConstructor { values } => {
                                    // get the children of both sides
                                    Some(self.get_implicit_array_length(*var_t.clone(), Some(Box::new(*values[0].clone()))))
                                }
                                _ => {
                                    self.error("unexpected token.");
                                    return Type::Void;
                                }
                            }
                        }
                    }
                    _ => None,
                };

                let mut output_type = *var_t; 
                if let Some(nested_array) = nested_array_option {
                    output_type = nested_array;
                }

                // if we dont have a nested array type, get the length of that array
                let Some(expr) = rhs else {
                    self.error("can not infer array length.");
                    return Type::Void;
                };

                match *expr {
                    Expression::ArrayConstructor { values } => {
                        // length of rhs is lenght of array
                        return Type::Array { t: Box::new(output_type), size: Some(values.len()) }
                    }
                    _ => {
                        self.error("unexpected token.");
                        return Type::Void;
                    }
                }
            }
            _ => array_type, 
        }
    }

    fn variable_declaration(&mut self, global: bool) -> Statement {
        let public = self.is_public();

        if global && public {
            self.error("Only top level variable declarations can be public.");
        }

        let mut constant = false;
        let mut variable_type: Option<Type> = None;
        let mut initial_value: Option<Box<Expression>> = None;
        if self.consume() == lexer::Token::Const {
            constant = true;
        }

        if global && !constant {
            self.error("global declarations must be constant");
            return Statement::ParseError;
        }

        let expr = self.primary();
        let identifier = self.unwrap_identifier(expr, "expected identifier after variable declaration.");

        if self.match_advance(&[lexer::Token::Colon]) {
            variable_type = Some(self.parse_type());
        }

        if self.match_advance(&[lexer::Token::Equal]) {
            initial_value = Some(Box::new(self.expression()));
        }

        if let Some(t) = variable_type {
            variable_type = Some(self.get_implicit_array_length(t, initial_value.clone()));
        }

        self.expect(lexer::Token::Semicolon, "expected semicolon after variable declaration.");

        return Statement::VariableDeclaration {
            identifier,
            variable_type,
            initial_value,
            constant,
            public,
            global,
        };
    }

    fn statement(&mut self) -> Statement {
        match self.current() {
            lexer::Token::LCurly => self.block(),
            lexer::Token::Let | lexer::Token::Const => self.variable_declaration(false),
            lexer::Token::While => self.while_statement(),
            lexer::Token::If => self.if_statement(),
            lexer::Token::Return => self.return_statement(),
            _ => self.expression_statement(),
        }
    }

    fn block(&mut self) -> Statement {
        let mut out: Vec<Box<Statement>> = Vec::new();
        self.advance();
        while self.current() != lexer::Token::RCurly {
            out.push(Box::new(self.statement()));
        }

        self.expect(
            lexer::Token::RCurly,
            "expected closing curly brace after the end of a block.",
        );

        Statement::Block(out)
    }

    fn while_statement(&mut self) -> Statement {
        self.advance();
        let condition = self.expression();
        let block = self.block();
        Statement::WhileStatement {
            condition: Box::new(condition),
            block: Box::new(block),
        }
    }

    fn if_statement(&mut self) -> Statement {
        self.advance(); // ignores the if token
        let condition = self.expression();
        let block = self.block();
        let mut alt: Option<Box<Statement>> = None;

        if self.current() == lexer::Token::Else {
            self.advance();
            if self.current() == lexer::Token::If {
                alt = Some(Box::new(self.if_statement()));
            } else {
                alt = Some(Box::new(self.block()));
            }
        }

        Statement::IfStatement {
            condition: Box::new(condition),
            block: Box::new(block),
            alt,
        }
    }

    fn return_statement(&mut self) -> Statement {
        self.advance(); // consume return token

        let mut value: Option<Box<Expression>> = None;
        if self.current() == lexer::Token::Semicolon {
            self.advance();
        } else { // non empty return
            let expression = self.expression();
            self.expect(lexer::Token::Semicolon, "expected semicolon after return statement.");
            value = Some(Box::new(expression));
        }

        Statement::Return {
            value
        }
    }

    fn expression_statement(&mut self) -> Statement {
        let out = Statement::ExpressionStatement(self.expression());
        self.expect(lexer::Token::Semicolon, "expected semicolon.");
        return out;
    }
}

// expressions
impl Parser {
    pub fn parse(&mut self) -> Option<Vec<Statement>> {
        let mut program: Vec<Statement> = Vec::new();
        while !self.at_end() {
            let statement = self.declaration();
            program.push(statement);
        };

        if self.errored {
            return None;
        }

        Some(program)
    }

    fn expression(&mut self) -> Expression {
        self.assignment()
    }

    fn assignment(&mut self) -> Expression {
        let mut expr = self.logical_or();

        if self.match_advance(&[
            lexer::Token::Equal,
            lexer::Token::PlusEqual,
            lexer::Token::MinusEqual,
            lexer::Token::StarEqual,
            lexer::Token::SlashEqual,
            lexer::Token::PercentEqual,
            lexer::Token::PipeEqual,
            lexer::Token::AmpersandEqual,
            lexer::Token::CaretEqual,
            lexer::Token::DoubleLeftCaret,
            lexer::Token::DoubleRightCaret,
        ]) {
            let operator = self.previous();
            let rhs = self.expression();

            if operator == lexer::Token::Equal {
                expr = Expression::Assignment {
                    identifier: Box::new(expr),
                    value: Box::new(rhs),
                };
            } else {
                let value = self.create_binary(expr.clone(), operator, rhs);
                expr = Expression::Assignment {
                    identifier: Box::new(expr),
                    value: Box::new(value),
                };
            }
        }

        return expr;
    }

    fn logical_or(&mut self) -> Expression {
        let mut expr = self.logical_and();

        while self.match_advance(&[lexer::Token::DoublePipe]) {
            let operator = self.previous();
            let rhs = self.logical_and();
            expr = self.create_binary(expr, operator, rhs);
        }

        return expr;
    }

    fn logical_and(&mut self) -> Expression {
        let mut expr = self.comparison();

        while self.match_advance(&[lexer::Token::DoubleAmpersand]) {
            let operator = self.previous();
            let rhs = self.comparison();
            expr = self.create_binary(expr, operator, rhs);
        }

        return expr;
    }

    fn comparison(&mut self) -> Expression {
        let mut expr = self.bitwise();

        if self.match_advance(&[
            lexer::Token::LeftCaret,
            lexer::Token::LeftCaretEqual,
            lexer::Token::RightCaret,
            lexer::Token::RightCaretEqual,
            lexer::Token::EqualEqual,
            lexer::Token::BangEqual,
        ]) {
            let operator = self.previous();
            let rhs = self.bitwise();
            expr = self.create_binary(expr, operator, rhs);
        }

        return expr;
    }

    fn bitwise(&mut self) -> Expression {
        let mut expr = self.bitshift();

        while self.match_advance(&[
            lexer::Token::Ampersand,
            lexer::Token::Pipe,
            lexer::Token::Caret,
        ]) {
            let operator = self.previous();
            let rhs = self.bitshift();
            expr = self.create_binary(expr, operator, rhs);
        }

        return expr;
    }

    fn bitshift(&mut self) -> Expression {
        let mut expr = self.term();

        while self.match_advance(&[
            lexer::Token::DoubleLeftCaret,
            lexer::Token::DoubleRightCaret,
        ]) {
            let operator = self.previous();
            let rhs = self.term();
            expr = self.create_binary(expr, operator, rhs);
        }

        return expr;
    }

    fn term(&mut self) -> Expression {
        let mut expr = self.factor();

        while self.match_advance(&[lexer::Token::Plus, lexer::Token::Minus]) {
            let operator = self.previous(); 
            let rhs = self.factor();
            expr = self.create_binary(expr, operator, rhs);
        }

        return expr;
    }

    fn factor(&mut self) -> Expression {
        let mut expr = self.unary();

        while self.match_advance(&[
            lexer::Token::Star,
            lexer::Token::Slash,
            lexer::Token::Percent,
        ]) {
            let operator = self.previous();
            let rhs = self.unary();
            expr = self.create_binary(expr, operator, rhs);
        }

        return expr;
    }

    fn unary(&mut self) -> Expression {
        if self.match_advance(&[
            lexer::Token::Bang,
            lexer::Token::Tilde,
            lexer::Token::Minus,
            lexer::Token::Ampersand,
        ]) {
            let operator = self.previous();
            let rhs = self.unary();
            return self.create_unary(operator, rhs);
        }

        self.struct_constructor()
    }

    fn struct_constructor(&mut self) -> Expression {
        let mut expr = self.postfix();

        match expr.clone() {
            Expression::Identifier(identifier) => {
                // ambigious syntax, make sure this isnt the pattern
                // if foo {
                //      ...
                // }
                // or:
                // while foo {
                //      ...
                // }
                if let Some(token) = self.peek(-1) {
                    if [lexer::Token::If, lexer::Token::While].contains(&token) {
                        return expr;
                    }
                }

                let mut members: Vec<Box<Expression>> = Vec::new();
                let mut comma: bool = true;

                if self.current() == lexer::Token::LCurly {
                    self.advance();

                    while self.current() != lexer::Token::RCurly {
                        if !comma {
                            self.error(
                                "expected comma seperating member values in struct constructor",
                            );
                        }

                        let member_identifier = self.struct_constructor();
                        let member_name = self.unwrap_identifier(
                            member_identifier,
                            "expected identifier in struct constructor.",
                        );

                        self.expect(lexer::Token::Colon, "expected colon in struct constructor");

                        let val = Box::new(self.expression());

                        comma = false;
                        if self.current() == lexer::Token::Comma {
                            self.advance();
                            comma = true;
                        }

                        members.push(Box::new(Expression::StructMember {
                            parent: identifier.clone(),
                            identifier: member_name,
                            val,
                        }));
                    }
                    self.advance(); // consume RParen

                    expr = Expression::StructConstructor {
                        identifier,
                        members,
                    };
                }
            }
            _ => {}
        }

        return expr;
    }

    fn postfix(&mut self) -> Expression {
        let mut expr = self.primary();
        while self.token_match(&[
            lexer::Token::DotStar,
            lexer::Token::Dot,
            lexer::Token::LParen,
            lexer::Token::LBrace,
        ]) {
            expr = match self.current() {
                lexer::Token::DotStar => {
                    self.advance(); // consume operator
                    self.create_unary(lexer::Token::DotStar, expr)
                }
                lexer::Token::Dot => {
                    self.advance();
                    let rhs_expr = self.primary();
                    let rhs = self.unwrap_identifier(rhs_expr, "rhs of dot operator must be an identifier");

                    Expression::Dot {
                        lhs: Box::new(expr),
                        rhs: rhs,
                    }
                }
                lexer::Token::LParen => {
                    self.advance(); // consume LParen

                    let mut args: Vec<Box<Expression>> = Vec::new();
                    while self.current() != lexer::Token::RParen {
                        args.push(Box::new(self.expression()));
                        if self.current() == lexer::Token::Comma {
                            self.advance();
                            continue;
                        }

                        // hey this shouldnt be a self.expect()
                        // dont make that mistake again pretty please
                        // (cost me half an hour of debugging btw!)
                        if self.current() != lexer::Token::RParen {
                            self.error("expected ',' or ')'");
                        }
                        break;
                    }

                    // Desugaring!
                    // foo.bar() -> foo.bar(&foo)
                    match expr.clone() {
                        Expression::Dot { lhs, rhs: _ } => {
                            args.insert(0, Box::new(Expression::Unary { 
                                operator: lexer::Token::Ampersand, 
                                member: lhs.clone(),
                            }));
                        }
                        _ => {}
                    };

                    // consume RParen
                    self.advance();
                    Expression::FunctionCall {
                        identifier: Box::new(expr),
                        args,
                    }
                }
                lexer::Token::LBrace => {
                    self.advance(); // consume LBrace
                    let index = Box::new(self.expression());

                    self.expect(
                        lexer::Token::RBrace,
                        "expected closing bracket for array access.",
                    );

                    Expression::ArrayAccess {
                        identifier: Box::new(expr),
                        index,
                    }
                }
                _ => unreachable!(),
            }
        }

        return expr;
    }

    fn primary(&mut self) -> Expression {
        let token = self.consume();
        match token {
            lexer::Token::Null => Expression::Null,
            lexer::Token::IntLit(val) => Expression::Int(val),
            lexer::Token::FloatLit(val) => Expression::Float(val),
            lexer::Token::Bool(val) => Expression::Bool(val),
            lexer::Token::StringLit(val) => Expression::String(val),
            lexer::Token::Char(val) => Expression::Char(val),
            lexer::Token::Identifier(val) => Expression::Identifier(val),
            lexer::Token::LParen => {
                let expr = self.expression();
                self.expect(
                    lexer::Token::RParen,
                    "expected closing ')' after expression.",
                );

                return expr;
            }
            lexer::Token::LBrace => {
                let mut values: Vec<Box<Expression>> = Vec::new();
                let mut comma = true;
                while self.current() != lexer::Token::RBrace {
                    if !comma {
                        self.error("expected comma");
                    }

                    let val = self.expression();
                    comma = false;
                    if self.current() == lexer::Token::Comma {
                        self.advance();
                        comma = true;
                    }

                    values.push(Box::new(val));
                }

                self.advance(); // consumes RBrace

                if values.len() == 0 {
                    self.error("array constructor must have more than 0 elements.");
                }

                Expression::ArrayConstructor { values }
            }
            _ => {
                self.error("expected expression.");
                return Expression::Null;
            }
        }
    }
}
