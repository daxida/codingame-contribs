use std::io;

fn main() {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    let mut s: Vec<i32> = input
        .split_whitespace()
        .map(|n| n.parse::<i32>().unwrap())
        .collect();
    for _ in 0..2000 {
        let next: i32 = s.iter().sum::<i32>() % s.len() as i32;
        s.push(next);
    }

    println!("{}", s.last().unwrap());
}
