use crate::parser;
use std::collections::HashMap;
use std::vec::Vec;

pub struct Generator {
    if_branch_count: usize,
    else_branch_count: usize,
    while_loop_count: usize,
    // l_count: usize,
    t_count: usize,
    out: String,
}

fn get_operator(operator: parser::Operator) -> &'static str {
    match operator {
        parser::Operator::Add => "+",
        parser::Operator::Subtract => "-",
        parser::Operator::Multiply => "*",
        parser::Operator::Divide => "/",
        parser::Operator::Modulus => "%",
        parser::Operator::BitwiseOr => "|",
        parser::Operator::BitwiseXOr => "^",
        parser::Operator::BitwiseAnd => "&",
        parser::Operator::BitwiseLeftShift => "<<",
        parser::Operator::BitwiseRightShift => ">>",
        parser::Operator::LogicalOr => "||",
        parser::Operator::LogicalAnd => "&&",
        parser::Operator::Equal => "==",
        parser::Operator::NotEqual => "!=",
        parser::Operator::LessThan => "<",
        parser::Operator::GreaterThan => ">",
        parser::Operator::LessThanEqual => "<=",
        parser::Operator::GreaterThanEqual => ">=",
        _ => panic!("unrecognized operaator"),
    }
}

impl Generator {
    pub fn new() -> Self {
        let mut sizes: HashMap<parser::Type, usize> = HashMap::new();
        sizes.insert(parser::Type::I8, 1);
        sizes.insert(parser::Type::U8, 1);
        sizes.insert(parser::Type::I16, 2);
        sizes.insert(parser::Type::U16, 2);
        sizes.insert(parser::Type::I32, 4);
        sizes.insert(parser::Type::U32, 4);
        sizes.insert(parser::Type::I64, 8);
        sizes.insert(parser::Type::U64, 8);
        sizes.insert(parser::Type::F16, 2);
        sizes.insert(parser::Type::F32, 4);
        sizes.insert(parser::Type::F64, 8);
        sizes.insert(parser::Type::Bool, 1);
        sizes.insert(parser::Type::Char, 4);
        
        Self {
            // symbol_tables: Vec::new(),
            // sizes,
            // l_count: 0,
            if_branch_count: 0,
            else_branch_count: 0,
            while_loop_count: 0,
            t_count: 0,
            out: String::new(),
        }
    }

    fn new_tmp(&mut self) -> String {
        self.t_count += 1;
        std::format!("t{}", self.t_count - 1)
    }

    fn tac_expr(&mut self, expr: parser::Expression) -> String {
        match expr {
            parser::Expression::Int(val) => val.to_string(),
            parser::Expression::Float(val) => val.to_string(),
            parser::Expression::Bool(val) => val.to_string(),
            parser::Expression::Identifier(val) => val,
            parser::Expression::Unary { operator, member } => {
                let nval = self.tac_expr(*member);
                let tmp = self.new_tmp();
                
                self.out.push_str(&std::format!("\t{} := {}{}\n", tmp, get_operator(operator), nval));
                return tmp;
            }
            parser::Expression::Binary { lhs, operator, rhs } => {
                let lval = self.tac_expr(*lhs);
                let rval = self.tac_expr(*rhs);
                
                let tmp = self.new_tmp();
                self.out.push_str(&std::format!("\t{} := {} {} {}\n", tmp, lval, get_operator(operator), rval));
                return tmp;
            }
            parser::Expression::FunctionCall { identifier, args } => {
                for arg in args {
                    let tmp = self.tac_expr(*arg);
                    self.out.push_str(&std::format!("\tparam {}\n", tmp));
                }
                match *identifier {
                    parser::Expression::Identifier(val) => {
                        let tmp = self.new_tmp();
                        self.out.push_str(&std::format!("\t{} := CALL {}\n", tmp, val));
                        return tmp;
                    }
                    _ => self.tac_expr(*identifier)
                }
            }
            parser::Expression::Assignment { identifier, value } => {
                let rval = self.tac_expr(*value);
                let lhs = self.tac_expr(*identifier);
                self.out.push_str(&std::format!("\t{} := {}\n", lhs, rval));
                return "".to_owned();
            }
            parser::Expression::Null => unimplemented!(), 
            parser::Expression::Char(_) => unimplemented!(), 
            parser::Expression::String(_) => unimplemented!(), 
            parser::Expression::ArrayAccess {
                identifier,
                index,
            } => unimplemented!(), 
            // TODO: again, may remove the idea of this entirely and do it through desugaring  
            parser::Expression::ArrayConstructor { values } => {
                unreachable!()
            },
            parser::Expression::StructMember { parent: _, identifier, val } => unimplemented!(),
            parser::Expression::StructConstructor { identifier, members } => unimplemented!(),
            parser::Expression::Dot { lhs, rhs } => {
                todo!();   
            }
        }
    }

    fn tac_gen(&mut self, statement: parser::Statement) {
        match statement {
            parser::Statement::FunctionDeclaration { 
                name, 
                return_type: _, 
                parameters: _, 
                body, 
                public: _ 
            } => {
                self.out.push_str(&(name + ":\n"));
                self.tac_gen(*body); 
            }
            parser::Statement::Return { value } => {
                let out = self.tac_expr(*value);
                self.out.push_str(&std::format!("\treturn {}\n", out));
            }
            parser::Statement::Block(statements) => {
                for s in statements {
                    self.tac_gen(*s);
                }
            }
            parser::Statement::VariableDeclaration { 
                identifier, 
                variable_type, 
                initial_value, 
                constant, 
                public, 
                global 
            } => {
                if let Some(s) = initial_value {
                    match *s.clone() {
                        parser::Expression::ArrayConstructor { values } => {
                            self.out.push_str(&std::format!("\t{}\n", identifier.clone()));
                            for (i, value) in values.iter().enumerate() {
                                let tmp = self.tac_expr(*value.clone()); 
                                self.out.push_str(&std::format!("\t{}[{}] := {}\n", identifier, i, tmp));
                            }
                        }
                        _ => {
                            let tmp = self.tac_expr(*s);
                            self.out.push_str(&std::format!("\t{} := {}\n", identifier, tmp));
                        } 
                    }
                } else {
                    self.out.push_str(&std::format!("\t{}\n", identifier));
                }
            }
            parser::Statement::IfStatement { condition, block, alt } => {
                let condition_expr = self.tac_expr(*condition);
                let end_if_branch = std::format!("end_if_{}", self.if_branch_count);
                self.if_branch_count += 1;

                if let Some(else_branch) = alt {
                    let else_label = std::format!("else_branch_{}", self.else_branch_count);
                    self.else_branch_count += 1;

                    self.out.push_str(&std::format!("\tifZ {} goto {}\n", condition_expr, else_label.clone())); 
                    self.tac_gen(*block);
                    self.out.push_str(&std::format!("\tgoto {}\n", end_if_branch.clone())); // merges branches

                    self.out.push_str(&(else_label + ":\n"));
                    self.tac_gen(*else_branch);

                } else {
                    self.out.push_str(&std::format!("\tifZ {} goto {}\n", condition_expr, end_if_branch.clone())); 
                    self.tac_gen(*block);
                }

                self.out.push_str(&std::format!("{}:\n", end_if_branch));
            }
            parser::Statement::WhileStatement { condition, block } => {
                let condition_expr = self.tac_expr(*condition);
                let while_loop_start = std::format!("loop_start_{}", self.while_loop_count);
                let while_loop_end = std::format!("loop_end_{}", self.while_loop_count);
                self.while_loop_count += 1;

                self.out.push_str(&(while_loop_start.clone() + ":\n"));
                self.out.push_str(&std::format!("\tifZ {} goto {}\n", condition_expr, while_loop_end));
                self.tac_gen(*block);
                self.out.push_str(&std::format!("\tgoto {}\n", while_loop_start));
                self.out.push_str(&(while_loop_end + ":\n"));
            }
            // parser::Statement::ElseStatement { body } => {
            //     self.tac_gen(*body);
            // }
            parser::Statement::ExpressionStatement(expr) => {
                self.tac_expr(*expr);
            }
            _ => {}
        }
    }

    pub fn generate(&mut self, ast: Vec<parser::Statement>) -> String {
        for statement in ast {
            self.tac_gen(statement);
        }

        return self.out.clone();
    }
}
