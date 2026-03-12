use crate::lexer;
use crate::util;
use std::collections::HashMap;
use std::vec::Vec;

// TODO: error sync does not work

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Pointer(Box<Type>),
    Array {
        t: Box<Type>,
        size: Option<usize>, // this will always be known when passed to codegen
    },
    Struct(String),
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
    lines: Vec<String>,
    src_path: String,
    errored: bool,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub identifier: String,
    pub val: Expression,
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
        identifier: Box<Expression>,
        args: Vec<Box<Expression>>,
    },
    ArrayAccess {
        identifier: Box<Expression>,
        index: Box<Expression>,
    },
    ArrayConstructor {
        values: Vec<Box<Expression>>,
    },
    StructConstructor {
        identifier: String,
        members: Vec<Member>,
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
        initial_value: Option<Expression>,
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
        value: Option<Expression>,
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
        condition: Expression,
        block: Box<Statement>,
        alt: Option<Box<Statement>>,
    },
    WhileStatement {
        condition: Expression,
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
        if self.i == 0 || self.at_end() {
            return lexer::Token::Null;
        }

        self.input[self.i - 1].token_type.clone()
    }

    fn current(&self) -> lexer::Token {
        if self.at_end() {
            return lexer::Token::Null;
        }

        self.input[self.i].token_type.clone()
    }

    // // can peek backwards too
    // fn peek(&self, index: i32) -> Option<lexer::Token> {
    //     let i = self.i as i32 + index - 1;
    //
    //     if i >= self.input.len() as i32 || i < 0 {
    //         return None;
    //     }
    //
    //     Some(self.input[i as usize].token_type.clone())
    // }

    fn next(&mut self) -> lexer::Token {
        self.i += 1;
        self.previous()
    }

    fn get_debug_token(&self) -> &lexer::DebugToken {
        if self.i == 0 || self.at_end() {
            return &self.input[0];
        }

        return &self.input[self.i - 1];
    }

    fn token_match(&self, tokens: &[lexer::Token]) -> bool {
        tokens.contains(&self.current())
    }

    // matches current and advances if true
    fn match_advance(&mut self, tokens: &[lexer::Token]) -> Option<lexer::Token> {
        let c = self.current();
        if tokens.contains(&c) {
            self.advance();
            return Some(c);
        }

        return None;
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

    fn consume(&mut self, token: lexer::Token, msg: Option<&str>) -> Option<lexer::Token> {
        if !self.at_end() && self.current() == token {
            self.advance();
            return Some(token);
        }

        if let Some(custom) = msg {
            self.error(custom);
        } else {
            self.error(&std::format!("expected {}", token));
        }

        return None;
    }

    fn expect_identifier(&mut self, msg: &str) -> String {
        if !self.at_end() {
            if let lexer::Token::Identifier(name) = self.current() {
                self.advance();
                return name;
            }
        }

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
                        self.error("unallowed expression");
                        return 0;
                    }
                }
            }
            _ => {
                self.error("unallowed expression");
                return 0;
            }
        }
    }

    fn parse_type(&mut self) -> Type {
        match self.current() {
            lexer::Token::LBrace => {
                self.advance();
                let mut size: Option<usize> = None;
                if self.current() == lexer::Token::Underscore {
                    self.advance();
                } else {
                    let expr = self.term();
                    let array_size = self.parse_array_expr(expr);
                    if array_size < 1 {
                        self.error("array size must be larger than 0");
                    }

                    size = Some(array_size as usize);
                }

                self.consume(lexer::Token::RBrace, None);
                return Type::Array {
                    t: Box::new(self.parse_type()),
                    size,
                };
            } 
            lexer::Token::Star => {
                self.advance(); // consumes the star
                Type::Pointer(Box::new(self.parse_type()))
            }
            lexer::Token::Identifier(identifier) => { // TODO: hey tihs doesnt work actually
                self.advance();
                if let Some(t) = self.types.get(&identifier) {
                    return t.clone();
                } else {
                    // TODO: this isnt going to always be a struct dumbass!
                    return Type::Struct(identifier);
                }
            }
            _ => {
                self.error("expected type");
                return Type::Void;
            }
        }
    }
}

impl Parser {
    fn error(&mut self, msg: &str) {
        if self.panic {
            return;
        }

        let token = self.get_debug_token();
        util::print_error(token, &self.lines, &self.src_path, msg);

        self.panic = true;
        self.errored = true;
    }

    fn synchronize_statement(&mut self) {
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
                | lexer::Token::Let
                | lexer::Token::Const
                | lexer::Token::Return 
                | lexer::Token::RCurly => {
                    return;
                }
                _ => self.advance(),
            }
        }
    }

    fn synchronize_declarations(&mut self) {
        while !self.at_end() {
            match self.current() {
                lexer::Token::Fn
                | lexer::Token::Struct
                | lexer::Token::Enum => break,
                _ => self.advance(),
            }
        }
    }
}

// statements
impl Parser {
    fn declaration(&mut self) -> Statement {
        if self.current() == lexer::Token::Pub {
            self.advance();
        }

        let out = match self.current() {
            lexer::Token::Fn => self.function_declaration(),
            lexer::Token::Struct => self.struct_declaration(),
            // lexer::Token::Enum => self.enum_declaration(),
            // TODO: i think i would much much rather have a static keyword instead
            lexer::Token::Let | lexer::Token::Const => self.variable_declaration(true),
            _ => {
                self.error("unrecognized top level expression");
                Statement::ParseError
                // vec![Statement::ParseError]
            }
        };

        if self.panic {
            self.synchronize_declarations();
        }

        return out;
    }

    fn function_declaration(&mut self) -> Statement {
        let public = self.is_public();

        self.advance(); // consume fn token
        let name = self.expect_identifier("expected function name");
        self.consume(lexer::Token::LParen, None);

        let mut parameters: Vec<Box<Statement>> = Vec::new();
        while self.current() != lexer::Token::RParen {
            let param_name = self.expect_identifier("expected parameter");
            self.consume(lexer::Token::Colon, None);

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
                self.error("expected comma");
            }
        }

        self.consume(lexer::Token::RParen, None);
        let return_type = self.parse_type();

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

        let struct_name = self.expect_identifier("expected struct name");

        if self.consume(lexer::Token::LCurly, None).is_none() {
            return Statement::ParseError;
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
                self.error("expected ','");
            }

            let public: bool = self.is_public();
            self.advance(); // consume the identifier
            self.consume(lexer::Token::Colon, Some("exected ':'"));

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

        self.consume(lexer::Token::RCurly, None);

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
 
    fn get_implicit_array_length(&mut self, input_type: Type, rhs: Option<Expression>) -> Type {
        let Type::Array { mut t, size } = input_type.clone() else {
            return input_type;
        };

        if !matches!(*t, Type::Array { .. }) && size.is_some() {
            return input_type;
        }

        let Some(expr) = rhs else {
            self.error("expected rhs");
            return Type::Void;
        };

        let Expression::ArrayConstructor { values } = expr else {
            self.error("expected array constructor");
            return Type::Void;
        };

        if let Type::Array { .. } = *t {
            t = Box::new(self.get_implicit_array_length(*t, Some(*values[0].clone())));
        }

        if let Some(declared_size) = size {
            if declared_size != values.len() {
                self.error("declared size does not match size of array constructor");
            }
        }

        return Type::Array { t, size: Some(values.len()) }
    }

    fn variable_declaration(&mut self, global: bool) -> Statement {
        let public = self.is_public();

        if !global && public {
            self.error("only top level variable declarations can be public");
        }

        let mut constant = false;
        let mut variable_type: Option<Type> = None;
        let mut initial_value: Option<Expression> = None;
        if self.next() == lexer::Token::Const {
            constant = true;
        }

        if global && !constant {
            self.error("global declarations must be constant");
            return Statement::ParseError;
        }

        let identifier = self.expect_identifier("expected identifier");

        if self.current() == lexer::Token::Colon {
            self.advance();
            variable_type = Some(self.parse_type());
        }

        if self.current() == lexer::Token::Equal {
            self.advance();
            initial_value = Some(self.expression());
        }

        if let Some(t) = variable_type {
            variable_type = Some(self.get_implicit_array_length(t, initial_value.clone()));
        }

        self.consume(lexer::Token::Semicolon, None);

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
        let out = match self.current() {
            lexer::Token::LCurly => self.block(),
            lexer::Token::Let | lexer::Token::Const => self.variable_declaration(false),
            lexer::Token::While => self.while_statement(),
            lexer::Token::If => self.if_statement(),
            lexer::Token::Return => self.return_statement(),
            _ => self.expression_statement(),
        };

        if self.panic {
            self.synchronize_statement();
            self.panic = false;
        }

        return out;
    }

    fn block(&mut self) -> Statement {
        let mut out: Vec<Box<Statement>> = Vec::new();
        self.advance();
        while self.current() != lexer::Token::RCurly && !self.at_end() {
            out.push(Box::new(self.statement()));
        }

        self.consume(lexer::Token::RCurly, None);
        Statement::Block(out)
    }

    fn while_statement(&mut self) -> Statement {
        self.advance();
        let condition = self.expression();
        let block = self.block();
        Statement::WhileStatement {
            condition: condition,
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
            condition: condition,
            block: Box::new(block),
            alt,
        }
    }

    fn return_statement(&mut self) -> Statement {
        self.advance(); // consume return token

        let mut value: Option<Expression> = None;
        if self.current() == lexer::Token::Semicolon {
            self.advance();
        } else { // non empty return
            let expression = self.expression();
            self.consume(lexer::Token::Semicolon, None);
            value = Some(expression);
        }

        Statement::Return {
            value
        }
    }

    fn expression_statement(&mut self) -> Statement {
        let out = Statement::ExpressionStatement(self.expression());
        self.consume(lexer::Token::Semicolon, None);
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

        if let Some(operator) = self.match_advance(&[
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
        ][..]) {
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

        while let Some(operator) = self.match_advance(&[lexer::Token::DoublePipe][..]) {
            let rhs = self.logical_and();
            expr = self.create_binary(expr, operator, rhs);
        }

        return expr;
    }

    fn logical_and(&mut self) -> Expression {
        let mut expr = self.comparison();

        while let Some(operator) = self.match_advance(&[lexer::Token::DoubleAmpersand][..]) {
            let rhs = self.comparison();
            expr = self.create_binary(expr, operator, rhs);
        }

        return expr;
    }

    fn comparison(&mut self) -> Expression {
        let mut expr = self.bitwise();

        if let Some(operator) = self.match_advance(&[
            lexer::Token::LeftCaret,
            lexer::Token::LeftCaretEqual,
            lexer::Token::RightCaret,
            lexer::Token::RightCaretEqual,
            lexer::Token::EqualEqual,
            lexer::Token::BangEqual,
        ][..]) {
            let rhs = self.bitwise();
            expr = self.create_binary(expr, operator, rhs);
        }

        return expr;
    }

    fn bitwise(&mut self) -> Expression {
        let mut expr = self.bitshift();

        while let Some(operator) = self.match_advance(&[
            lexer::Token::Ampersand,
            lexer::Token::Pipe,
            lexer::Token::Caret,
        ][..]) {
            let rhs = self.bitshift();
            expr = self.create_binary(expr, operator, rhs);
        }

        return expr;
    }

    fn bitshift(&mut self) -> Expression {
        let mut expr = self.term();

        while let Some(operator) = self.match_advance(&[
            lexer::Token::DoubleLeftCaret,
            lexer::Token::DoubleRightCaret,
        ][..]) {
            let rhs = self.term();
            expr = self.create_binary(expr, operator, rhs);
        }

        return expr;
    }

    fn term(&mut self) -> Expression {
        let mut expr = self.factor();

        while let Some(operator) = self.match_advance(&[lexer::Token::Plus, lexer::Token::Minus][..]) {
            let rhs = self.factor();
            expr = self.create_binary(expr, operator, rhs);
        }

        return expr;
    }

    fn factor(&mut self) -> Expression {
        let mut expr = self.unary();

        while let Some(operator) = self.match_advance(&[
            lexer::Token::Star,
            lexer::Token::Slash,
            lexer::Token::Percent,
        ][..]) {
            let rhs = self.unary();
            expr = self.create_binary(expr, operator, rhs);
        }

        return expr;
    }

    fn unary(&mut self) -> Expression {
        if let Some(operator) = self.match_advance(&[
            lexer::Token::Bang,
            lexer::Token::Tilde,
            lexer::Token::Minus,
            lexer::Token::Ampersand,
        ][..]) {
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
                let token = self.previous();
                if [lexer::Token::If, lexer::Token::While].contains(&token) {
                    return expr;
                }

                // let mut members: Vec<Box<Expression>> = Vec::new();
                let mut members: Vec<Member> = Vec::new();
                let mut comma: bool = true;

                if self.current() == lexer::Token::LCurly {
                    self.advance();

                    while self.current() != lexer::Token::RCurly {
                        if !comma {
                            self.error(
                                "expected comma seperating member values in struct constructor",
                            );
                        }

                        let member_name = self.expect_identifier("expected identifier in struct constructor");
                        self.consume(lexer::Token::Colon, None);
                        let val = self.expression();

                        comma = false;
                        if self.current() == lexer::Token::Comma {
                            self.advance();
                            comma = true;
                        }

                        members.push(Member {
                            identifier: member_name,
                            val
                        })
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
        ][..]) {
            expr = match self.current() {
                lexer::Token::DotStar => {
                    self.advance(); // consume operator
                    return Expression::Unary { 
                        operator: lexer::Token::DotStar, 
                        member: Box::new(expr) 
                    }
                }
                lexer::Token::Dot => {
                    self.advance();
                    // let rhs_expr = self.primary();
                    let rhs = self.expect_identifier("rhs of dot operator must be an identifier");
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
                    self.consume(lexer::Token::RBrace, None);

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
        let token = self.next();
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
                self.consume(lexer::Token::RParen, None);

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
