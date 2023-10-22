use std::io;

fn sort_chars(s: &str) -> Vec<char> {
    let mut s_sorted: Vec<char> = s.chars().collect();
    s_sorted.sort();
    s_sorted
}

fn solve(a: &str, b: &str, fixed: &str) -> bool {
    // Same length and same chars
    if sort_chars(a) != sort_chars(b) { return false; }

    // Same positions for fixed chars
    let same_positions = a.chars().zip(b.chars()).all(|(c1, c2)| {
        !(fixed.contains(c1) || fixed.contains(c2)) || c1 == c2
    });
    if !same_positions { return false; }

    // Same chars in "islands"
    let aa: Vec<&str> = a.split(|c| fixed.contains(c)).collect();
    let bb: Vec<&str> = b.split(|c| fixed.contains(c)).collect();
    let same_chars_in_islands = aa.iter().zip(bb.iter()).all(|(s1, s2)| {
        sort_chars(s1) == sort_chars(s2)
    });

    same_chars_in_islands
}

fn main() {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let t: i32 = input_line.trim().parse().unwrap();

    for _ in 0..t {
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let inputs: Vec<&str> = input_line.split_whitespace().collect();
        let base = inputs[0].trim();
        let target = inputs[1].trim();
        let fixed = inputs[2].trim();

        let ans = solve(base, target, fixed);
        println!("{}", ans);
    }
}