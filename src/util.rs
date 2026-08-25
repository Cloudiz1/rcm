use crate::lexer;
use crate::parser::Type;

pub fn print_error(token: &lexer::DebugToken, lines: &Vec<String>, src_path: &String, msg: &str) {
    println!("at {}:{}", src_path, token.line_number + 1);
    println!("{} | {}", token.line_number + 1, lines[token.line_number]);
    for _ in 0..token.column {
        print!(" ");
    }

    let line_len = (token.line_number + 1).to_string().len();
    for _ in 0..line_len {
        print!(" ");
    }

    println!("   ^ {}", msg);
    println!("");
}

pub fn get_size(t: &Type) -> usize {
    match &t {
        Type::Str
        | Type::Pointer(_)
        | Type::Array{ .. }
        | Type::Usize => std::mem::size_of::<usize>(),
        Type::I8 => 1,
        Type::U8 => 1,
        Type::I16 => 2,
        Type::U16 => 2,
        Type::I32 => 4,
        Type::U32 => 4,
        Type::I64 => 8,
        Type::U64 => 8,
        Type::F16 => 2,
        Type::F32 => 4,
        Type::F64 => 8,
        Type::Char => 1,
        Type::Bool => 1,
        Type::Void => 0,
        Type::Unknown => 0,
        // TODO: recusively call get_size and sum
        Type::Struct(ident) => todo!(),
    }
} 
