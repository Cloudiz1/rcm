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

// TODO: operation should be its own enum...
#[derive(Debug)]
pub enum AST {
    Null,
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Unary {
        operator: Operator,
        rhs: Box<AST>,
    },
    Postfix {
        lhs: Box<AST>,
        operator: Operator,
    },
    Binary {
        lhs: Box<AST>,
        operator: Operator,
        rhs: Box<AST>,
    },
}

pub struct Parser {
    input: Vec<lexer::Token>,
    i: usize,
}

impl Parser {
    pub fn new() -> Self {
        Parser {
            input: Vec::new(),
            i: 0,
        }
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

    // fn peek(&self) -> lexer::Token {
    //     if self.i >= self.input.len() - 1 {
    //         panic!("self.peek() tried to access out of bounds index.");
    //     }
    //
    //     self.input[self.i + 1].clone()
    // }

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

    // // TODO: Might want to turn this into an option type for better error handling later on
    // fn get_operator(&self, token: lexer::Token) -> Operator {
    //     match token {
    //         lexer::Token::DoublePipe => Operator::Or,
    //         lexer::Token::DoubleAmpersand => Operator::And,
    //         lexer::Token::Caret => Operator
    //         _ => panic!("Unknown operator."),
    //     }
    // }

    fn create_binary(&self, lhs: AST, operator: Operator, rhs: AST) -> AST {
        AST::Binary {
            lhs: Box::from(lhs),
            operator,
            rhs: Box::from(rhs),
        }
    }

    fn create_unary(&self, operator: Operator, rhs: AST) -> AST {
        AST::Unary {
            operator,
            rhs: Box::from(rhs),
        }
    }

    fn create_postfix(&self, lhs: AST, operator: Operator) -> AST {
        AST::Postfix {
            lhs: Box::from(lhs),
            operator,
        }
    }

    fn expression(&mut self) -> AST {
        self.logical_or()
    }

    fn logical_or(&mut self) -> AST {
        let mut expr = self.logical_and();

        while self.match_advance(&[lexer::Token::DoublePipe]) {
            let rhs = self.logical_and();
            expr = self.create_binary(expr, Operator::Or, rhs);
        }

        return expr;
    }

    fn logical_and(&mut self) -> AST {
        let mut expr = self.comparison();

        while self.match_advance(&[lexer::Token::DoubleAmpersand]) {
            let rhs = self.comparison();
            expr = self.create_binary(expr, Operator::And, rhs);
        }

        return expr;
    }

    fn comparison(&mut self) -> AST {
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

    fn bitwise(&mut self) -> AST {
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

    fn bitshift(&mut self) -> AST {
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

    fn term(&mut self) -> AST {
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

    fn factor(&mut self) -> AST {
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

    fn unary(&mut self) -> AST {
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

    fn postfix(&mut self) -> AST {
        let mut expr = self.primary();

        while self.match_advance(&[
            // TODO: HEY THE DOT OPERATOR IS A BINARY OPEATOR DINGUS
            lexer::Token::DotStar,
            lexer::Token::LParen,
            lexer::Token::LBrace,
        ]) {
            let operator = match self.previous() {
                lexer::Token::DotStar => Operator::Dereference,
                lexer::Token::LParen => Operator::Unimplemented,
                lexer::Token::LBrace => Operator::ArrayAccess,
                _ => panic!(""),
            };

            expr = self.create_postfix(expr, operator);
        }

        return expr;
    }

    fn primary(&mut self) -> AST {
        let token = self.consume();

        match token {
            lexer::Token::IntLit(i) => AST::Int(i),
            lexer::Token::FloatLit(f) => AST::Float(f),
            lexer::Token::Bool(b) => AST::Bool(b),
            lexer::Token::StringLit(s) => AST::String(s),
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

    pub fn parse(&mut self, input: Vec<lexer::Token>) -> AST {
        self.input = input;
        self.expression()
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
pub fn print_ast(input: AST) {
    match input {
        AST::Null => print!("NULL"),
        AST::Int(val) => print!("{val}"),
        AST::Float(val) => print!("{val}"),
        AST::Bool(val) => print!("{val}"),
        AST::String(val) => print!("{val}"),
        AST::Binary { lhs, operator, rhs } => {
            print!("(");
            print_operator(operator);
            print!(" ");
            print_ast(*lhs);
            print!(" ");
            print_ast(*rhs);
            print!(")");
        }
        AST::Unary { operator, rhs } => {
            print!("(");
            print_operator(operator);
            print!(" ");
            print_ast(*rhs);
            print!(")");
        }
        AST::Postfix { lhs, operator } => {
            print!("(");
            print_operator(operator);
            print!(" ");
            print_ast(*lhs);
            print!(")");
        }
        _ => panic!("unimplemented AST print"),
    }
}
