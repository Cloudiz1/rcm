use std::collections::HashMap;
use std::vec::Vec;

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
    pub line: u32,
}

pub struct Tokenizer {
    input: String,
    i: usize,
    line: u32,
    keywords: HashMap<String, Token>,
}

impl Tokenizer {
    pub fn new() -> Self {
        let mut tokenizer = Tokenizer {
            i: 0,
            line: 1,
            input: String::new(),
            keywords: HashMap::new(),
        };

        tokenizer.keywords.insert(String::from("let"), Token::Let);
        tokenizer
            .keywords
            .insert(String::from("const"), Token::Const);
        tokenizer.keywords.insert(String::from("pub"), Token::Pub);
        tokenizer.keywords.insert(String::from("fn"), Token::Fn);
        tokenizer.keywords.insert(String::from("enum"), Token::Enum);
        tokenizer
            .keywords
            .insert(String::from("struct"), Token::Struct);
        tokenizer.keywords.insert(String::from("if"), Token::If);
        tokenizer.keywords.insert(String::from("else"), Token::Else);
        tokenizer
            .keywords
            .insert(String::from("switch"), Token::Switch);
        tokenizer.keywords.insert(String::from("case"), Token::Case);
        tokenizer
            .keywords
            .insert(String::from("while"), Token::While);
        tokenizer.keywords.insert(String::from("do"), Token::Do);
        tokenizer.keywords.insert(String::from("for"), Token::For);
        tokenizer
            .keywords
            .insert(String::from("break"), Token::Break);
        tokenizer
            .keywords
            .insert(String::from("continue"), Token::Continue);
        tokenizer
            .keywords
            .insert(String::from("return"), Token::Return);
        tokenizer
            .keywords
            .insert(String::from("true"), Token::Bool(true));
        tokenizer
            .keywords
            .insert(String::from("false"), Token::Bool(false));
        tokenizer.keywords.insert(String::from("null"), Token::Null);

        tokenizer
    }

    fn peek(&self) -> Option<char> {
        if self.i >= self.input.len() - 1 {
            return None;
        }

        Some(self.input.chars().nth(self.i + 1).unwrap())
    }

    fn advance(&mut self) {
        self.i += 1;
    }

    fn is_next_equal(&mut self, t1: Token, t2: Token) -> Token {
        if self.peek().unwrap() == '=' {
            self.advance();
            return t2;
        }

        t1
    }

    fn is_double(&mut self, c: char) -> bool {
        if let Some(n) = self.peek() {
            if n == c {
                return true;
            }
        }

        return false;
    }

    fn get_escaped_char(&self) -> Option<char> {
        // TODO: maybe erroring in here is better? would require less logic to figure out if its an
        // invalid escape or unrecognized escape
        let Some(n) = self.peek() else {
            return None;
        };

        Some(match n {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\'' => '\'',
            '\"' => '\"',
            _ => return None,
        })
    }

    fn read_character(&mut self) -> Option<Token> {
        let Some(n) = self.peek() else {
            // TODO: Error, trailing single quote
            return None;
        };

        if n == '\'' {
            // TODO: Error, Empty character literal
            return None;
        }

        let mut out: Token = Token::Char(n);
        if n == '\\' {
            self.advance();
            if let Some(esc_char) = self.get_escaped_char() {
                out = Token::Char(esc_char);
            } else {
                // TODO: Error, improperly escaped character
                // might be able to leave this erroring to `self.get_escaped_char()`?
            }
        }

        self.advance(); // consume read character
        let Some(n) = self.peek() else {
            // TODO: Error, no closing single quote
            return None;
        };

        if n != '\'' {
            // TODO: Error, no closing single quote
            // you can actually make this more specific as in a) too many
            // characeters in a char or b) no single closing single quote
            return None;
        }

        self.advance(); // consume single quote
        return Some(out);
    }

    fn read_string(&mut self) -> Option<Token> {
        let mut out: String = String::new();
        while let Some(n) = self.peek() {
            if n == '\"' {
                break;
            }

            if n == '\\' {
                self.advance();
                if let Some(esc_char) = self.get_escaped_char() {
                    out.push(esc_char);
                } else {
                    // TODO: invalid escaped char
                    return None;
                }
            } else {
                out.push(n);
            }

            self.advance();
        }

        // TODO: Error, no closing delimeter
        self.advance();
        Some(Token::StringLit(out))
    }

    fn read_identifiers_and_keywords(&mut self, curr: char) -> Option<Token> {
        let mut out: String = String::from(curr);
        while let Some(n) = self.peek() {
            if !n.is_ascii_alphanumeric() && n != '_' {
                break;
            }

            out.push(n);
            self.advance();
        }

        if let Some(keyword) = self.keywords.get(&out) {
            return Some(keyword.clone());
        }

        Some(Token::Identifier(out))
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

    fn get_token(&mut self, c: char) -> Option<Token> {
        // I dont think this should error, all of its helper functions should instead

        if c == '\n' {
            self.line += 1;
        }

        if c.is_whitespace() {
            return None;
        }

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
            '=' => self.is_next_equal(Token::Equal, Token::EqualEqual),
            '+' => self.is_next_equal(Token::Plus, Token::PlusEqual),
            '-' => self.is_next_equal(Token::Minus, Token::MinusEqual),
            '*' => self.is_next_equal(Token::Star, Token::StarEqual),
            '/' => {
                if let Some(next_c) = self.peek() {
                    if next_c == '/' {
                        self.advance();
                        while let Some(n) = self.peek() {
                            self.advance();
                            if n == '\n' {
                                self.line += 1;
                                return None;
                            }
                        }
                    }
                }

                return Some(self.is_next_equal(Token::Slash, Token::SlashEqual));
            }
            '%' => self.is_next_equal(Token::Percent, Token::PercentEqual),
            '!' => self.is_next_equal(Token::Bang, Token::BangEqual),
            '~' => Token::Tilde,
            '_' => Token::Underscore,
            '^' => self.is_next_equal(Token::Caret, Token::CaretEqual),
            '|' => {
                if self.is_double(c) {
                    self.advance();
                    return Some(Token::DoublePipe);
                } else {
                    return Some(self.is_next_equal(Token::Pipe, Token::PipeEqual));
                }
            }
            '&' => {
                if self.is_double(c) {
                    self.advance();
                    return Some(Token::DoubleAmpersand);
                } else {
                    return Some(self.is_next_equal(Token::Ampersand, Token::AmpersandEqual));
                }
            }
            '>' => {
                if self.is_double(c) {
                    self.advance();

                    if let Some(n) = self.peek() {
                        if n == '=' {
                            self.advance();
                            return Some(Token::DoubleRightCaretEqual);
                        }
                    }

                    return Some(Token::DoubleRightCaret);
                } else {
                    return Some(self.is_next_equal(Token::RightCaret, Token::RightCaretEqual));
                }
            }
            '<' => {
                if self.is_double(c) {
                    self.advance();

                    if let Some(n) = self.peek() {
                        if n == '=' {
                            self.advance();
                            return Some(Token::DoubleLeftCaretEqual);
                        }
                    }

                    return Some(Token::DoubleLeftCaret);
                } else {
                    return Some(self.is_next_equal(Token::LeftCaret, Token::LeftCaretEqual));
                }
            }
            '\'' => return self.read_character(),
            '\"' => return self.read_string(),
            'a'..='z' | 'A'..='Z' => return self.read_identifiers_and_keywords(c),
            '0'..='9' => return self.read_number(c),
            _ => Token::Unknown(c),
        })
    }

    pub fn tokenize(&mut self, input: String) -> Vec<DebugToken> {
        let mut out: Vec<DebugToken> = Vec::new();
        self.input = input;
        while self.i < self.input.len() {
            if let Some(t) = self.get_token(self.input.chars().nth(self.i).unwrap()) {
                let debug_token = DebugToken {
                    line: self.line,
                    token_type: t,
                };

                out.push(debug_token);
            }
            self.advance();
        }

        let eof = DebugToken {
            token_type: Token::EOF,
            line: self.line,
        };

        out.push(eof);
        out
    }
}

pub fn print_tokens(tokens: Vec<DebugToken>) {
    let mut line = 0;
    for token in tokens {
        if token.line != line {
            println!("");
            line = token.line;
            print!("{} | ", line);
        }

        print!(" {:?}", token.token_type);
    }

    println!("");
    println!("");
}

// TODO: i can 100% reuse these tests if i just write a helper that ignores the line numbers for me

// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     #[test]
//     fn basic_symbols() {
//         let tokens = Tokenizer::new().tokenize("()[]{},.;:".to_owned());
//         let expected_tokens = Vec::from(&[
//             Token::LParen,
//             Token::RParen,
//             Token::LBrace,
//             Token::RBrace,
//             Token::LCurly,
//             Token::RCurly,
//             Token::Comma,
//             Token::Dot,
//             Token::Semicolon,
//             Token::Colon,
//             Token::EOF,
//         ]);
//
//         assert_eq!(tokens, expected_tokens);
//     }
//
//     #[test]
//     fn operations() {
//         let tokens = Tokenizer::new().tokenize("+ += - -= * *= / /= % %=".to_owned());
//         let expected_tokens = Vec::from(&[
//             Token::Plus,
//             Token::PlusEqual,
//             Token::Minus,
//             Token::MinusEqual,
//             Token::Star,
//             Token::StarEqual,
//             Token::Slash,
//             Token::SlashEqual,
//             Token::Percent,
//             Token::PercentEqual,
//             Token::EOF,
//         ]);
//
//         assert_eq!(tokens, expected_tokens);
//     }
//
//     #[test]
//     fn bitwise() {
//         let tokens = Tokenizer::new().tokenize("& &= | |= ^ ^= << <<= >> >>=".to_owned());
//         let expected_tokens = Vec::from(&[
//             Token::Ampersand,
//             Token::AmpersandEqual,
//             Token::Pipe,
//             Token::PipeEqual,
//             Token::Caret,
//             Token::CaretEqual,
//             Token::DoubleLeftCaret,
//             Token::DoubleLeftCaretEqual,
//             Token::DoubleRightCaret,
//             Token::DoubleRightCaretEqual,
//             Token::EOF,
//         ]);
//
//         assert_eq!(tokens, expected_tokens);
//     }
//
//     #[test]
//     fn logical() {
//         let tokens = Tokenizer::new().tokenize("|| &&".to_owned());
//         let expected_tokens = Vec::from(&[Token::DoublePipe, Token::DoubleAmpersand, Token::EOF]);
//         assert_eq!(tokens, expected_tokens);
//     }
//
//     #[test]
//     fn literals() {
//         let tokens = Tokenizer::new().tokenize("\"Hello, world!\" 42 67.41".to_owned());
//         let expected_tokens = Vec::from(&[
//             Token::StringLit("Hello, world!".to_owned()),
//             Token::IntLit(42),
//             Token::FloatLit(67.41),
//             Token::EOF,
//         ]);
//         assert_eq!(tokens, expected_tokens);
//     }
//
//     #[test]
//     fn variable_declaration() {
//         let tokens = Tokenizer::new().tokenize("let foo: i32 = 1 + 2 * 3;".to_owned());
//         let expected_tokens = Vec::from(&[
//             Token::Let,
//             Token::Identifier("foo".to_owned()),
//             Token::Colon,
//             Token::Identifier("i32".to_owned()),
//             Token::Equal,
//             Token::IntLit(1),
//             Token::Plus,
//             Token::IntLit(2),
//             Token::Star,
//             Token::IntLit(3),
//             Token::Semicolon,
//             Token::EOF,
//         ]);
//         assert_eq!(tokens, expected_tokens);
//     }
// }
