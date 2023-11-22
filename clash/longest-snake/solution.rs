use std::io;

fn main() {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let n = input_line.trim().parse::<i32>().unwrap();
    let mut snake_box = String::new();
    for i in 0..n as usize {
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let row = input_line.trim_matches('\n').to_string();
        if i % 2 == 1 {
            snake_box += &row.chars().rev().collect::<String>();
        } else {
            snake_box += &row;
        }
    }

    let max_length: usize = snake_box
        .split(|c| c == '<' || c == '>')
        .map(|snake| snake.len() + 1)
        .max()
        .unwrap();

    println!("{}", max_length);
}
