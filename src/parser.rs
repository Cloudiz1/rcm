use crate::lexer;
use std::collections::HashMap;
use std::vec::Vec;

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,

    BitwiseOr,
    BitwiseAnd,
    BitwiseXOr,
    BitwiseLeftShift,
    BitwiseRightShift,

    Not,
    Negate,

    Or,
    And,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,

    ArrayAccess,
    Reference,
    Dereference,
    Dot,

    Assignment,

    Unimplemented,
}

#[derive(Debug, Clone)]
pub enum Type {
    Pointer(Box<Type>),
    Array { t: Box<Type>, size: Box<Expression> },
    Struct(String),
    Enum(String),
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    Isize,
    Usize,
    F16,
    F32,
    F64,
    Bool,
    Void,
    Str,
    Char,
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
        operator: Operator,
        member: Box<Expression>,
    },
    Postfix {
        lhs: Box<Expression>,
        operator: Operator,
    },
    Binary {
        lhs: Box<Expression>,
        operator: Operator,
        rhs: Box<Expression>,
    },
    Assignment {
        identifier: String,
        value: Box<Expression>,
    },
    FunctionCall {
        identifier: Box<Expression>, // in case i want function pointers or closures (foo())()
        args: Vec<Box<Expression>>,
    },
    ArrayAccess {
        identifier: Box<Expression>, // TODO: support foo[0][0]
        index: Box<Expression>,
    },
    StructMember {
        identifier: String,
        val: Box<Expression>,
    },
    StructConstructor {
        identifier: String,
        members: Vec<Box<Expression>>, // struct members
    },
}

#[derive(Debug)]
pub enum Statement {
    Program(Vec<Box<Statement>>),
    ExpressionStatement(Box<Expression>),
    Block(Vec<Box<Statement>>),
    VariableDeclaration {
        identifier: String,
        variable_type: Option<Type>,
        initial_value: Option<Box<Expression>>,
        is_constant: bool,
    },
    EnumDeclaration {
        name: String,
        varients: Vec<String>,
        public: bool,
    },
    Parameter {
        name: String,
        t: Type,
    },
    Return {
        value: Box<Expression>,
    },
    FunctionDeclaration {
        name: String,
        return_type: Type,
        parameters: Vec<Box<Statement>>, // vec of params
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

pub struct Parser {
    input: Vec<lexer::Token>,
    i: usize,
    types: HashMap<String, Type>,
}

// helpers
impl Parser {
    pub fn new() -> Self {
        let mut types: HashMap<String, Type> = HashMap::new();
        types.insert("i8".to_owned(), Type::I8);
        types.insert("u8".to_owned(), Type::U8);
        types.insert("i16".to_owned(), Type::I16);
        types.insert("u16".to_owned(), Type::U16);
        types.insert("i32".to_owned(), Type::I32);
        types.insert("u32".to_owned(), Type::U32);
        types.insert("i64".to_owned(), Type::I64);
        types.insert("u64".to_owned(), Type::U64);
        types.insert("isize".to_owned(), Type::Isize);
        types.insert("usize".to_owned(), Type::Usize);
        types.insert("f16".to_owned(), Type::F16);
        types.insert("f32".to_owned(), Type::F32);
        types.insert("f64".to_owned(), Type::F64);
        types.insert("bool".to_owned(), Type::Bool);
        types.insert("void".to_owned(), Type::Void);
        types.insert("string".to_owned(), Type::Str);
        types.insert("char".to_owned(), Type::Char);

        Self {
            input: Vec::new(),
            i: 0,
            types,
        }
    }

    fn advance(&mut self) {
        self.i += 1;
    }

    fn previous(&self) -> lexer::Token {
        if self.i == 0 {
            panic!("self.previous() called on index 0.");
        }

        self.input[self.i - 1].clone()
    }

    fn current(&self) -> lexer::Token {
        if self.i >= self.input.len() {
            panic!("self.current() tried to access out of bounds index.");
        }

        self.input[self.i].clone()
    }

    fn peek(&self) -> lexer::Token {
        if self.i >= self.input.len() - 1 {
            panic!("self.peek() tried to access out of bounds index.");
        }

        self.input[self.i + 1].clone()
    }

    fn consume(&mut self) -> lexer::Token {
        self.i += 1;
        self.previous()
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

    fn create_binary(&self, lhs: Expression, operator: Operator, rhs: Expression) -> Expression {
        Expression::Binary {
            lhs: Box::new(lhs),
            operator,
            rhs: Box::new(rhs),
        }
    }

    fn create_unary(&self, operator: Operator, member: Expression) -> Expression {
        Expression::Unary {
            operator,
            member: Box::new(member),
        }
    }

    /*
     * consumes token
     */
    fn expect(&mut self, token: lexer::Token, msg: &str) -> bool {
        if self.consume() != token {
            print!("found: ");
            self.print();
            self.advance();
            self.print();
            panic!("{msg}");
        }

        return true;
    }

    fn unwrap_identifier(&mut self, i: Expression, msg: &str) -> String {
        match i {
            Expression::Identifier(identifier) => identifier,
            _ => panic!("{msg}"),
        }
    }

    fn is_public(&mut self) -> bool {
        if self.i == 0 {
            return false;
        }

        self.previous() == lexer::Token::Pub
    }

    fn parse_type(&mut self) -> Type {
        match self.current() {
            // TODO: this quick implementation has a lot left to be desired
            // id like to have an implicit size, let foo: [_]u8 = {'a', 'b', 'c'}, for example
            // id like to have a slice type as well, fn foo(input: []u8) should be valid
            // its probably worth it to have strings as just char * and chars as u8s
            // but in the same vein a non null terminated string seems nice
            lexer::Token::LBrace => {
                self.advance(); // consume LBrace
                let size = self.term();
                self.expect(lexer::Token::RBrace, "expected closing brace.");
                Type::Array {
                    t: Box::new(self.parse_type()),
                    size: Box::new(size),
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
                }

                println!("{:?}", identifier);
                panic!("unrecognized type.");
            }
            _ => panic!("expected type."),
        }
    }

    fn print(&self) {
        lexer::print_token(self.current());
    }
}

// statements
impl Parser {
    fn declaration(&mut self) -> Statement {
        if self.current() == lexer::Token::Pub {
            self.advance();
        }

        match self.current() {
            lexer::Token::Fn => self.function_declaration(),
            lexer::Token::Struct => self.struct_declaration(),
            lexer::Token::Enum => self.enum_declaration(),
            _ => self.statement(),
        }
    }

    fn function_declaration(&mut self) -> Statement {
        let public = self.is_public();

        self.advance(); // consume fn token
        let mut identifier = self.primary();
        let name =
            self.unwrap_identifier(identifier, "expected function name in after keyword 'fn'");

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

            match self.current() {
                lexer::Token::Identifier(_) => panic!("missing comma between function parameters."),
                _ => {}
            }
        }

        self.expect(lexer::Token::RParen, "expected right paren");

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

    // TODO: Currently allows zero-sized structs, not sure if i want this or not
    fn struct_declaration(&mut self) -> Statement {
        let struct_public = self.is_public();
        self.advance(); // consume struct token

        let identifier = self.primary();
        let struct_name =
            self.unwrap_identifier(identifier, "expected struct name after struct keyword.");
        self.expect(lexer::Token::LCurly, "expected body of struct.");

        self.types
            .insert(struct_name.clone(), Type::Struct(struct_name.clone()));

        let mut members: Vec<Box<Statement>> = Vec::new();
        let mut comma: bool = true;
        loop {
            if self.current() == lexer::Token::Pub {
                self.advance();
                continue;
            }

            match self.current() {
                lexer::Token::Identifier(identifier) => {
                    if !comma {
                        panic!("expected comma between member declarations");
                    }

                    let public: bool = self.is_public();
                    self.advance(); // consume the identifier
                    self.expect(
                        lexer::Token::Colon,
                        "expected colon following member declaration",
                    );

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
                _ => break,
            };
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
                panic!("member declarations must go before methods.")
            }
            _ => {}
        }

        self.expect(
            lexer::Token::RCurly,
            "expected closing brace on struct declaration.",
        );

        Statement::StructDeclaration {
            name: struct_name,
            members,
            methods,
            public: struct_public,
        }
    }

    fn enum_declaration(&mut self) -> Statement {
        let public = self.is_public();
        self.advance(); // consume enum token
        let mut identifier = self.primary();
        let name = self.unwrap_identifier(identifier, "expected identifier in enum declaration.");
        self.types.insert(name.clone(), Type::Enum(name.clone()));

        let mut varients: Vec<String> = Vec::new();
        self.expect(lexer::Token::LCurly, "expected body in enum declaration.");
        while self.current() != lexer::Token::RCurly {
            identifier = self.primary();
            varients.push(self.unwrap_identifier(identifier, "expected varient in enum body."));

            let token = self.current();
            if token != lexer::Token::Comma && token != lexer::Token::RCurly {
                panic!("expected comma seperating enum varients.");
            }

            if token == lexer::Token::Comma {
                self.advance();
            }
        }

        self.expect(
            lexer::Token::RCurly,
            "expected closing right curly after enum declaration.",
        );

        Statement::EnumDeclaration {
            name,
            varients,
            public,
        }
    }

    fn variable_declaration(&mut self) -> Statement {
        let mut is_constant = false;
        let mut variable_type: Option<Type> = None;
        let mut initial_value: Option<Box<Expression>> = None;
        if self.consume() == lexer::Token::Const {
            is_constant = true;
        }

        let expr = self.primary();
        let identifier =
            self.unwrap_identifier(expr, "expected identifier after variable declaration.");

        if self.match_advance(&[lexer::Token::Colon]) {
            variable_type = Some(self.parse_type());
            // expr = self.primary();
            // variable_type = Some(self.unwrap_identifier(expr, "expected type after colon."));
        }

        if self.match_advance(&[lexer::Token::Equal]) {
            initial_value = Some(Box::new(self.expression()));
        }

        self.expect(
            lexer::Token::Semicolon,
            "expected semicolon after variable declaration.",
        );

        return Statement::VariableDeclaration {
            identifier,
            variable_type,
            initial_value,
            is_constant,
        };
    }

    fn statement(&mut self) -> Statement {
        match self.current() {
            lexer::Token::LCurly => self.block(),
            lexer::Token::Let | lexer::Token::Const => self.variable_declaration(),
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
        let expression = self.expression();
        self.expect(
            lexer::Token::Semicolon,
            "expected semicolon after return statement.",
        );
        Statement::Return {
            value: Box::new(expression),
        }
    }

    fn expression_statement(&mut self) -> Statement {
        let out = Box::new(self.expression());
        self.expect(
            lexer::Token::Semicolon,
            "expected semicolon after expression",
        );
        Statement::ExpressionStatement(out)
    }
}

// expressions
impl Parser {
    pub fn parse(&mut self, input: Vec<lexer::Token>) -> Statement {
        self.input = input;

        let mut program: Vec<Box<Statement>> = Vec::new();
        while self.current() != lexer::Token::EOF {
            program.push(Box::new(self.declaration()));
        }

        Statement::Program(program)
    }

    fn expression(&mut self) -> Expression {
        self.assignment()
    }

    fn assignment(&mut self) -> Expression {
        let operator = match self.peek() {
            lexer::Token::Equal => Some(Operator::Equal),
            lexer::Token::PlusEqual => Some(Operator::Add),
            lexer::Token::MinusEqual => Some(Operator::Subtract),
            lexer::Token::StarEqual => Some(Operator::Multiply),
            lexer::Token::SlashEqual => Some(Operator::Divide),
            lexer::Token::PercentEqual => Some(Operator::Modulus),
            lexer::Token::PipeEqual => Some(Operator::BitwiseOr),
            lexer::Token::AmpersandEqual => Some(Operator::BitwiseAnd),
            lexer::Token::CaretEqual => Some(Operator::BitwiseXOr),
            lexer::Token::DoubleLeftCaret => Some(Operator::BitwiseLeftShift),
            lexer::Token::DoubleRightCaret => Some(Operator::BitwiseRightShift),
            _ => None,
        };

        if let Some(unwrapped_operator) = operator {
            let expected_identifier = self.primary();
            let identifier =
                self.unwrap_identifier(expected_identifier, "lhs is not an identifier");

            self.advance(); // skip the operator
            let rhs = self.expression();

            if unwrapped_operator == Operator::Equal {
                return Expression::Assignment {
                    identifier,
                    value: Box::new(rhs),
                };
            } else {
                let value = self.create_binary(
                    Expression::Identifier(identifier.clone()),
                    unwrapped_operator,
                    rhs,
                );
                return Expression::Assignment {
                    identifier,
                    value: Box::new(value),
                };
            }
        }

        self.logical_or()
    }

    fn logical_or(&mut self) -> Expression {
        let mut expr = self.logical_and();

        while self.match_advance(&[lexer::Token::DoublePipe]) {
            let rhs = self.logical_and();
            expr = self.create_binary(expr, Operator::Or, rhs);
        }

        return expr;
    }

    fn logical_and(&mut self) -> Expression {
        let mut expr = self.comparison();

        while self.match_advance(&[lexer::Token::DoubleAmpersand]) {
            let rhs = self.comparison();
            expr = self.create_binary(expr, Operator::And, rhs);
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
            let operator = match self.previous() {
                lexer::Token::LeftCaret => Operator::LessThan,
                lexer::Token::LeftCaretEqual => Operator::LessThanEqual,
                lexer::Token::RightCaret => Operator::GreaterThan,
                lexer::Token::RightCaretEqual => Operator::GreaterThanEqual,
                lexer::Token::EqualEqual => Operator::Equal,
                lexer::Token::BangEqual => Operator::NotEqual,
                _ => unreachable!(),
            };

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
            let operator = match self.previous() {
                lexer::Token::Ampersand => Operator::BitwiseAnd,
                lexer::Token::Pipe => Operator::BitwiseOr,
                lexer::Token::Caret => Operator::BitwiseXOr,
                _ => unreachable!(),
            };

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
            let operator = match self.previous() {
                lexer::Token::DoubleLeftCaret => Operator::BitwiseLeftShift,
                lexer::Token::DoubleRightCaret => Operator::BitwiseRightShift,
                _ => unreachable!(),
            };

            let rhs = self.term();
            expr = self.create_binary(expr, operator, rhs);
        }

        return expr;
    }

    fn term(&mut self) -> Expression {
        let mut expr = self.factor();

        while self.match_advance(&[lexer::Token::Plus, lexer::Token::Minus]) {
            let operator = match self.previous() {
                lexer::Token::Plus => Operator::Add,
                lexer::Token::Minus => Operator::Subtract,
                _ => unreachable!(),
            };

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
            let operator = match self.previous() {
                lexer::Token::Star => Operator::Multiply,
                lexer::Token::Slash => Operator::Divide,
                lexer::Token::Percent => Operator::Modulus,
                _ => unreachable!(),
            };

            let rhs = self.unary();
            expr = self.create_binary(expr, operator, rhs);
        }

        return expr;
    }

    fn unary(&mut self) -> Expression {
        if self.match_advance(&[
            lexer::Token::Bang,
            lexer::Token::Minus,
            lexer::Token::Ampersand,
        ]) {
            let operator = match self.previous() {
                lexer::Token::Bang => Operator::Not,
                lexer::Token::Minus => Operator::Negate,
                lexer::Token::Ampersand => Operator::Reference,
                _ => unreachable!(),
            };

            let rhs = self.unary();
            return self.create_unary(operator, rhs);
        }

        self.struct_constructor()
    }

    fn struct_constructor(&mut self) -> Expression {
        let mut expr = self.postfix();

        match expr.clone() {
            Expression::Identifier(identifier) => {
                let mut members: Vec<Box<Expression>> = Vec::new();
                let mut comma: bool = true;

                if self.current() == lexer::Token::LCurly {
                    self.advance();

                    while self.current() != lexer::Token::RCurly {
                        if !comma {
                            panic!("expected comma seperating member values in struct constructor");
                        }

                        let member_identifier = self.primary();
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
                    self.create_unary(Operator::Dereference, expr)
                }
                lexer::Token::Dot => {
                    self.advance();
                    let rhs = self.primary();
                    self.create_binary(expr, Operator::Dot, rhs)
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

                        if self.current() != lexer::Token::RParen {
                            panic!("expected comma seperating arguments or closing parenthesis.");
                        }

                        break;
                    }

                    self.advance(); // consume RParen
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
            lexer::Token::IntLit(val) => Expression::Int(val),
            lexer::Token::FloatLit(val) => Expression::Float(val),
            lexer::Token::Bool(val) => Expression::Bool(val),
            lexer::Token::StringLit(val) => Expression::String(val),
            lexer::Token::Identifier(val) => Expression::Identifier(val),
            lexer::Token::LParen => {
                let expr = self.expression();
                self.expect(
                    lexer::Token::RParen,
                    "expected closing ')' after expression.",
                );

                return expr;
            }
            _ => {
                println!("found: {:?}", token);
                panic!("expected expression.");
            }
        }
    }
}
