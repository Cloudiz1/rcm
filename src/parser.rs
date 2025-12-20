use crate::lexer;
use std::vec::Vec;

#[derive(Debug, PartialEq)]
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
    Assignment {
        identifier: String,
        value: Box<Expression>,
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
}

// statements
impl Parser {
    fn declaration(&mut self) -> Statement {
        // match self.current() {
        //     lexer::Token::Let | lexer::Token::Const => self.variable_declaration(),
        //     _ => self.expression_statement();
        // }
        self.statement()
    }

    fn statement(&mut self) -> Statement {
        match self.current() {
            lexer::Token::LCurly => self.block(),
            lexer::Token::Let | lexer::Token::Const => self.variable_declaration(),
            lexer::Token::While => self.while_statement(),
            lexer::Token::If => self.if_statement(),
            _ => self.expression_statement(),
        }
    }

    fn expression_statement(&mut self) -> Statement {
        let out = Box::from(self.expression());
        self.check_semicolon("expected semicolon after expression.");
        Statement::ExpressionStatement(out)
    }

    fn block(&mut self) -> Statement {
        let mut out: Vec<Box<Statement>> = Vec::new();
        self.advance();
        while self.current() != lexer::Token::RCurly {
            out.push(Box::from(self.statement()));
        }

        if self.consume() != lexer::Token::RCurly {
            panic!("expected closing curly brace after the end of a block.");
        }

        Statement::Block(out)
    }

    fn variable_declaration(&mut self) -> Statement {
        let mut is_constant = false;
        let identifier: String;
        let mut variable_type: Option<String> = None;
        let mut initial_value: Option<Box<Expression>> = None;
        if self.consume() == lexer::Token::Const {
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

    fn while_statement(&mut self) -> Statement {
        self.advance();
        let condition = self.expression();
        let block = self.block();
        Statement::WhileStatement {
            condition: Box::from(condition),
            block: Box::from(block),
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
                alt = Some(Box::from(self.if_statement()));
            } else {
                alt = Some(Box::from(self.block()));
            }
        }

        Statement::IfStatement {
            condition: Box::from(condition),
            block: Box::from(block),
            alt,
        }
    }
}

// expressions
impl Parser {
    pub fn parse(&mut self, input: Vec<lexer::Token>) -> Statement {
        self.input = input;

        let mut program: Vec<Box<Statement>> = Vec::new();
        while self.current() != lexer::Token::EOF {
            program.push(Box::from(self.declaration()));
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
            _ => None,
        };

        if let Some(unwrapped_operator) = operator {
            let identifier = match self.consume() {
                lexer::Token::Identifier(val) => val,
                // TODO: yknow the drill...
                _ => panic!("lhs is not an identifier."),
            };

            self.advance(); // skip the operator
            let rhs = self.expression();

            if unwrapped_operator == Operator::Equal {
                return Expression::Assignment {
                    identifier,
                    value: Box::from(rhs),
                };
            } else {
                let value = self.create_binary(
                    Expression::Identifier(identifier.clone()),
                    unwrapped_operator,
                    rhs,
                );
                return Expression::Assignment {
                    identifier,
                    value: Box::from(value),
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
                panic!("expected expression.");
            }
        }
    }
}
