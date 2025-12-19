use crate::lexer;
use std::vec::Vec;

#[derive(Debug)]
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

    Assignment,

    Unimplemented,
}

#[derive(Debug)]
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
        rhs: Box<Expression>,
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
}

#[derive(Debug)]
pub enum Statement {
    Program(Vec<Box<Statement>>),
    ExpressionStatement(Box<Expression>),
    Block(Vec<Box<Statement>>),
    VariableDeclaration {
        identifier: String,
        variable_type: Option<String>,
        initial_value: Option<Box<Expression>>,
        is_constant: bool,
    },
    VariableAssignment {
        identifier: String,
        value: Box<Expression>,
    },
}

pub struct Parser {
    input: Vec<lexer::Token>,
    i: usize,
}

// helpers
impl Parser {
    pub fn new() -> Self {
        Self {
            input: Vec::new(),
            i: 0,
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

    // matches current and advances if true
    fn match_advance(&mut self, tokens: &[lexer::Token]) -> bool {
        if tokens.contains(&self.current()) {
            self.i += 1;
            return true;
        }

        return false;
    }

    fn create_binary(&self, lhs: Expression, operator: Operator, rhs: Expression) -> Expression {
        Expression::Binary {
            lhs: Box::from(lhs),
            operator,
            rhs: Box::from(rhs),
        }
    }

    fn create_unary(&self, operator: Operator, rhs: Expression) -> Expression {
        Expression::Unary {
            operator,
            rhs: Box::from(rhs),
        }
    }

    fn create_postfix(&self, lhs: Expression, operator: Operator) -> Expression {
        Expression::Postfix {
            lhs: Box::from(lhs),
            operator,
        }
    }

    fn check_semicolon(&mut self, msg: &str) {
        if self.current() != lexer::Token::Semicolon {
            panic!("{msg}");
        }

        self.advance();
    }

    // #[cfg(feature = "debug")]
    fn print_current(&self) {
        println!("{:?}", self.current());
    }
}

impl Parser {
    pub fn parse(&mut self, input: Vec<lexer::Token>) -> Statement {
        self.input = input;

        let mut program: Vec<Box<Statement>> = Vec::new();
        while self.current() != lexer::Token::EOF {
            program.push(Box::from(self.declaration()));
        }

        Statement::Program(program)
    }

    fn declaration(&mut self) -> Statement {
        if !self.match_advance(&[lexer::Token::Let, lexer::Token::Const]) {
            return self.statement();
        }

        let mut is_constant = false;
        let identifier: String;
        let mut variable_type: Option<String> = None;
        let mut initial_value: Option<Box<Expression>> = None;
        if self.previous() == lexer::Token::Const {
            is_constant = true;
        }

        match self.consume() {
            lexer::Token::Identifier(val) => {
                identifier = val;
            }
            _ => {
                // TODO: of course better error handling later on
                panic!("expected identifier after variable declaration.");
            }
        };

        if self.match_advance(&[lexer::Token::Colon]) {
            match self.consume() {
                lexer::Token::Identifier(val) => {
                    variable_type = Some(val);
                }
                _ => {
                    // TODO: of course better error handling later on
                    panic!("expected type after semicolon.");
                }
            };
        }

        if self.match_advance(&[lexer::Token::Equal]) {
            initial_value = Some(Box::from(self.expression()));
        }

        self.check_semicolon("expected semicolon after variable declaration.");
        return Statement::VariableDeclaration {
            identifier,
            variable_type,
            initial_value,
            is_constant,
        };
    }

    fn statement(&mut self) -> Statement {
        let out = Statement::ExpressionStatement(Box::from(self.expression()));
        self.check_semicolon("expected semicolon after expression statement");
        return out;
    }

    fn expression(&mut self) -> Expression {
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
                _ => panic!(""),
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
                _ => panic!(""),
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
                _ => panic!(""),
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
                _ => panic!(""),
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
                _ => panic!(""),
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
                _ => panic!(""),
            };

            let rhs = self.unary();
            return self.create_unary(operator, rhs);
        }

        self.postfix()
    }

    // TODO: this needs like crazy work
    fn postfix(&mut self) -> Expression {
        let mut expr = self.primary();

        while self.match_advance(&[
            lexer::Token::DotStar,
            lexer::Token::LParen,
            lexer::Token::LBrace,
        ]) {
            let operator = match self.previous() {
                lexer::Token::DotStar => Operator::Dereference,
                lexer::Token::LParen => Operator::Unimplemented, // function call
                lexer::Token::LBrace => Operator::ArrayAccess,
                _ => panic!(""),
            };

            expr = self.create_postfix(expr, operator);
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
                if self.current() != lexer::Token::RParen {
                    panic!("expected closing ')' after expression.");
                }

                // ignores RParen
                _ = self.consume();
                expr
            }
            _ => {
                println!("{:?}", token);
                panic!("expected expression.");
            }
        }
    }
}

#[cfg(feature = "debug")]
fn print_operator(operator: Operator) {
    match operator {
        Operator::Add => print!("+"),
        Operator::Subtract => print!("-"),
        Operator::Multiply => print!("*"),
        Operator::Divide => print!("/"),
        Operator::Modulus => print!("%"),
        Operator::BitwiseOr => print!("|"),
        Operator::BitwiseAnd => print!("&"),
        Operator::BitwiseXOr => print!("^"),
        Operator::BitwiseLeftShift => print!("<<"),
        Operator::BitwiseRightShift => print!(">>"),
        Operator::Not => print!("!"),
        Operator::Negate => print!("-"),
        Operator::Or => print!("||"),
        Operator::And => print!("&&"),
        Operator::Equal => print!("=="),
        Operator::NotEqual => print!("!="),
        Operator::LessThan => print!("<"),
        Operator::GreaterThan => print!(">"),
        Operator::LessThanEqual => print!("<="),
        Operator::GreaterThanEqual => print!(">="),
        Operator::ArrayAccess => print!("[]"),
        Operator::Reference => print!("&"),
        Operator::Dereference => print!(".*"),
        Operator::Assignment => print!("="),
        Operator::Unimplemented => panic!("unimplemented operation."),
    }
}

#[cfg(feature = "debug")]
pub fn print_ast(input: Expression) {
    match input {
        Expression::Null => print!("NULL"),
        Expression::Int(val) => print!("{val}"),
        Expression::Float(val) => print!("{val}"),
        Expression::Bool(val) => print!("{val}"),
        Expression::String(val) => print!("{val}"),
        Expression::Binary { lhs, operator, rhs } => {
            print!("(");
            print_operator(operator);
            print!(" ");
            print_ast(*lhs);
            print!(" ");
            print_ast(*rhs);
            print!(")");
        }
        Expression::Unary { operator, rhs } => {
            print!("(");
            print_operator(operator);
            print!(" ");
            print_ast(*rhs);
            print!(")");
        }
        Expression::Postfix { lhs, operator } => {
            print!("(");
            print_operator(operator);
            print!(" ");
            print_ast(*lhs);
            print!(")");
        }
        _ => panic!("unimplemented AST print"),
    }
}
