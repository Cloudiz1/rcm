use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // general syntax
    LParen,
    RParen,
    LBrace,
    RBrace,
    LCurly,
    RCurly,
    Dot,
    Comma,
    Semicolon,
    Colon,
    Equal,
    DotStar,
    Underscore,

    // keywords
    Let,
    Const,
    Fn,
    Pub,
    Struct,
    Enum,

    If,
    Else,
    Switch,
    Case,
    While,
    Do,
    For,
    Break,
    Continue,
    Return,

    // Arithmetic
    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    // Bitwise
    Tilde,            // ~
    Ampersand,        // &
    Pipe,             // |
    Caret,            // ^
    DoubleLeftCaret,  // <<
    DoubleRightCaret, // >>

    // Assignment
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,

    BangEqual,             // !=
    AmpersandEqual,        // &=
    PipeEqual,             // |=
    CaretEqual,            // ^=
    DoubleLeftCaretEqual,  // <<=
    DoubleRightCaretEqual, // >>=

    // logical
    Bang,            // !
    EqualEqual,      // ==
    DoubleAmpersand, // &&
    DoublePipe,      // ||
    LeftCaret,       // <
    RightCaret,      // >
    LeftCaretEqual,  // <=
    RightCaretEqual, // >=

    Char(char),
    StringLit(String),
    Identifier(String),
    IntLit(i64),
    FloatLit(f64),
    Bool(bool),
    EscChar(u8),

    // special characters
    Null,
    Newline,
    Tab,

    EOF,
}

#[derive(Clone, Debug)]
pub struct DebugToken {
    pub token_type: Token,
    pub line_number: usize,
    pub column: usize,
    // pub line: String, 
}

pub struct Tokenizer {
    input: Vec<char>,
    i: usize,
    keywords: HashMap<String, Token>,
    
    // error reporting 
    src_path: String,
    line_number: usize,
    column: usize,
    lines: Vec<String>,
    panic: bool,
    errored: bool,
}

impl Tokenizer {
    pub fn new() -> Self {
        let mut tokenizer = Tokenizer {
            input: Vec::new(),
            i: 0,
            keywords: HashMap::new(),

            // error reporting
            src_path: String::new(),
            line_number: 0,
            column: 0,
            lines: Vec::new(),
            panic: false,
            errored: false,
        };

        tokenizer.keywords.insert(String::from("let"), Token::Let);
        tokenizer.keywords.insert(String::from("const"), Token::Const);
        tokenizer.keywords.insert(String::from("pub"), Token::Pub);
        tokenizer.keywords.insert(String::from("fn"), Token::Fn);
        tokenizer.keywords.insert(String::from("enum"), Token::Enum);
        tokenizer.keywords.insert(String::from("struct"), Token::Struct);
        tokenizer.keywords.insert(String::from("if"), Token::If);
        tokenizer.keywords.insert(String::from("else"), Token::Else);
        tokenizer.keywords.insert(String::from("switch"), Token::Switch);
        tokenizer.keywords.insert(String::from("case"), Token::Case);
        tokenizer.keywords.insert(String::from("while"), Token::While);
        tokenizer.keywords.insert(String::from("do"), Token::Do);
        tokenizer.keywords.insert(String::from("for"), Token::For);
        tokenizer.keywords.insert(String::from("break"), Token::Break);
        tokenizer.keywords.insert(String::from("continue"), Token::Continue);
        tokenizer.keywords.insert(String::from("return"), Token::Return);
        tokenizer.keywords.insert(String::from("true"), Token::Bool(true));
        tokenizer.keywords.insert(String::from("false"), Token::Bool(false));
        tokenizer.keywords.insert(String::from("null"), Token::Null);

        return tokenizer;
    }

    fn error(&mut self, msg: &str) {
        if !self.panic {
            println!("at {}:{}:{}:", self.src_path, self.line_number + 1, self.column);
            println!("{} | {}", self.line_number + 1, self.lines[self.line_number]);
            for _ in 0..self.column {
                print!(" ");
            }

            let line_len = (self.line_number + 1).to_string().len();
            for _ in 0..=line_len {
                print!(" ");
            }
    
            println!("^ {}", msg);
            println!("");
        }

        self.panic = true;
        self.errored = true;
    }

    fn sync(&mut self) {
        while let Some(c) = self.current() {
            if c == ';' {
                self.panic = false;
                return;
            }

            self.advance();
        }
    }

    fn peek(&mut self) -> Option<char> {
        let index = self.i + 1;
        if index >= self.input.len() {
            self.error("expected token");
            return None;
        }

        return Some(self.input[index]);
    }

    fn current(&self) -> Option<char> {
        if self.i >= self.input.len() {
            return None;
        }

        return Some(self.input[self.i]);
    }

    fn advance(&mut self) {
        self.i += 1;
        self.column += 1;
    }

    fn operation_and_assignment(&mut self, a: Token, b: Token) -> Token {
        if let Some(c) = self.peek() {
            if c == '=' {
                return b;
            } 
        }

        return a;
    }

    fn is_double(&mut self, c: char) -> bool {
        if let Some(n) = self.peek() {
            return n == c;
        }

        return false;
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current() {
            if c == '\n' {
                self.line_number += 1;
                self.column = 0;
            }

            if !c.is_whitespace() {
                return;
            }

            self.advance();
        }
    }

    fn skip_comment(&mut self) {
        let Some(c) = self.current() else {
            return;
        };

        let Some(n) = self.peek() else {
            return;
        };

        if c != '/' || n != '/' {
            return
        }

        while let Some(c) = self.current() {
            self.advance();
            if c == '\n' {
                return;
            }
        }
    }

    fn get_escaped_char(&mut self) -> char {
        if let Some(c) = self.current() {
            match c {
                'n' => return '\n',
                't' => return '\t',
                'r' => return '\r',
                '\\' => return '\\',
                _ => {}
            }
        }

        self.error("unrecognized escape sequence");
        return '\0';
    }

    fn read_character(&mut self) -> Option<Token> {
        let Some(n) = self.peek() else {
            self.error("expected token after single quote");
            return None;
        };
        
        if n == '\'' {
            self.error("empty character literal");
            return None;
        }

        let mut out: Token = Token::Char(n);
        if n == '\\' {
            self.advance();
            out = Token::Char(self.get_escaped_char()); // this handles error
        }

        self.advance(); // consumes read character
        let Some(n) = self.peek() else {
            self.error("expected token");
            return None;
        };

        if n != '\'' {
            self.error("expected closing single quote");
            return None;
        }

        self.advance();
        return Some(out);
    }

    fn read_string(&mut self) -> Option<Token> {
        let mut out = String::new();
        while let Some(n) = self.peek() {
            if n == '\"' || n == '\n' {
                break;
            }

            let mut c = n;
            if n == '\\' {
                self.advance();
                c = self.get_escaped_char();
                self.advance();
            } 

            out.push(c);
            self.advance()
        };

        let Some(n) = self.peek() else {
            self.error("expected token");
            return None;
        };

        if n != '\"' {
            self.error("expected closing quote");
            return None;
        }

        self.advance();
        return Some(Token::StringLit(out));
    }

    fn read_word(&mut self, curr: char) -> Option<Token> {
        let mut out: String = String::from(curr);
        while let Some(n) = self.peek() {
            if !n.is_ascii_alphanumeric() && n != '_' {
                break;
            };

            out.push(n);
            self.advance();
        }

        if let Some(keyword) = self.keywords.get(&out) {
            return Some(keyword.clone());
        }

        return Some(Token::Identifier(out));
    }

    fn read_number(&mut self, curr: char) -> Option<Token> {
        let mut out: String = String::from(curr);
        let mut is_float: bool = false;
        while let Some(n) = self.peek() {
            if n == '.' {
                is_float = true;
            } else if !n.is_numeric() {
                break;
            }

            out.push(n);
            self.advance();
        }

        if is_float {
            Some(Token::FloatLit(out.parse::<f64>().unwrap()))
        } else {
            Some(Token::IntLit(out.parse::<i64>().unwrap()))
        }
    }

    fn get_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        self.skip_comment();

        let Some(c) = self.current() else {
            return None;
        };

        Some(match c {
            '(' => Token::LParen,
            ')' => Token::RParen,
            '[' => Token::LBrace,
            ']' => Token::RBrace,
            '{' => Token::LCurly,
            '}' => Token::RCurly,
            '.' => {
                if let Some(next_c) = self.peek() {
                    if next_c == '*' {
                        self.advance();
                        return Some(Token::DotStar);
                    }
                } 
                return Some(Token::Dot);
            }
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            ':' => Token::Colon,
            '=' => self.operation_and_assignment(Token::Equal, Token::EqualEqual),
            '+' => self.operation_and_assignment(Token::Plus, Token::PlusEqual),
            '-' => self.operation_and_assignment(Token::Minus, Token::MinusEqual),
            '*' => self.operation_and_assignment(Token::Star, Token::StarEqual),
            '/' => self.operation_and_assignment(Token::Slash, Token::SlashEqual),
            '%' => self.operation_and_assignment(Token::Percent, Token::PercentEqual),
            '!' => self.operation_and_assignment(Token::Bang, Token::BangEqual),
            '~' => Token::Tilde,
            '_' => Token::Underscore,
            '^' => self.operation_and_assignment(Token::Caret, Token::CaretEqual),
            '|' => {
                if self.is_double(c) {
                    self.advance();
                    return Some(Token::DoublePipe);
                } else {
                    return Some(self.operation_and_assignment(Token::Pipe, Token::PipeEqual));
                }
            }
            '&' => {
                if self.is_double(c) {
                    self.advance();
                    return Some(Token::DoubleAmpersand);
                } else {
                    return Some(self.operation_and_assignment(Token::Ampersand, Token::AmpersandEqual));
                }
            }
            '>' => {
                if self.is_double(c) {
                    self.advance();
                    return Some(self.operation_and_assignment(Token::DoubleRightCaret, Token::DoubleRightCaretEqual));
                } else {
                    return Some(self.operation_and_assignment(Token::RightCaret, Token::RightCaretEqual));
                }
            }
            '<' => {
                if self.is_double(c) {
                    self.advance();
                    return Some(self.operation_and_assignment(Token::DoubleLeftCaret, Token::DoubleLeftCaretEqual));
                } else {
                    return Some(self.operation_and_assignment(Token::LeftCaret, Token::LeftCaretEqual));
                }
            }
            '\'' => return self.read_character(),
            '\"' => return self.read_string(),
            'a'..='z' | 'A'..='Z' => return self.read_word(c),
            '0'..='9' => return self.read_number(c),
            _ => {
                self.error("unrecognized token");
                return None;
            }
        })
    }

    pub fn tokenize(&mut self, input: String, src_path: String) -> Option<Vec<DebugToken>> {
        let mut out: Vec<DebugToken> = Vec::new();
        self.lines = input.split("\n").map(|x| x.trim().to_owned()).collect();
        self.input = input.chars().collect::<Vec<char>>();
        self.src_path = src_path;
        while self.i < self.input.len() {
            if let Some(token_type) = self.get_token() {
                let line_number = self.line_number;
                out.push( DebugToken {
                    token_type,
                    line_number,
                    column: self.column,
                    // line: self.lines[line_number].clone(),
                });
            }

            if self.panic {
                self.sync();
            }

            self.advance();
        }

        if self.errored {
            return None;
        }

        return Some(out);
    }
}

pub fn print_tokens(tokens: Vec<DebugToken>) {
    let mut line = 1;
    for token in tokens {
        if token.line_number != line {
            println!("");
            line = token.line_number;
            print!("{} | ", line + 1);
        }

        print!(" {:?}", token.token_type);
    }

    println!("");
    println!("");
}
