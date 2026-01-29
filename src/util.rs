use crate::lexer;

pub fn print_error(token: lexer::DebugToken, lines: &Vec<String>, src_path: &String, msg: &str) {
    // dbg!(&token);
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
