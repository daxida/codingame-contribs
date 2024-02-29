use std::collections::HashSet;
use std::io;

fn solve<'a>(original: &str, words: Vec<&'a str>, used: &mut Vec<&'a str>, sols: &mut Vec<String>) {
    if original.is_empty() && words.is_empty() {
        let solution: String = used.iter().cloned().collect::<Vec<_>>().join(" ");
        sols.push(solution);
        if sols.len() > 1 {
            println!("Unsolvable");
            std::process::exit(0);
        }
    }

    let uniq: HashSet<&str> = words.clone().into_iter().collect();

    for word in uniq {
        if original.starts_with(word) {
            let mut new_words = words.clone();
            let index = new_words.iter().position(|&w| w == word).unwrap();
            new_words.remove(index);
            used.push(word);
            solve(&original[word.len()..], new_words, used, sols);
            used.pop();
        }
    }
}

fn main() {
    let mut original = String::new();
    io::stdin().read_line(&mut original).unwrap();
    let original = original.trim();

    let mut raw_words = String::new();
    io::stdin().read_line(&mut raw_words).unwrap();
    let words: Vec<&str> = raw_words.trim().split_whitespace().collect();

    let mut sols = Vec::new();
    let mut used = Vec::new();
    solve(original, words, &mut used, &mut sols);

    println!("{}", sols[0]);
}