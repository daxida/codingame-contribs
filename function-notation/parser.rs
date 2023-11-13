use nom::{
    branch::alt, 
    bytes::complete::*, 
    character::complete::*, 
    combinator::*, 
    multi::*, 
    sequence::*,
    IResult,
};
use std::io;

#[derive(Debug, Clone)]
enum Expr {
    Add(Box<Expr>, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Var(String),
    Par(Box<Expr>),
}

fn sym<'a>(input: &'a str) -> impl Fn(&'a str) -> IResult<&'a str, String> {
    move |i: &'a str| {
        map(delimited(space0, tag(input), space0), |s: &'a str| {
            s.to_string()
        })(i)
    }
}

fn var(input: &str) -> IResult<&str, Expr> {
    map(alpha1, |s: &str| Expr::Var(s.to_string()))(input)
}

fn paren(input: &str) -> IResult<&str, Expr> {
    map(delimited(sym("("), expr, sym(")")), |e: Expr| {
        Expr::Par(Box::new(e))
    })(input)
}

fn factor(input: &str) -> IResult<&str, Expr> {
    alt((paren, var))(input)
}

fn term(input: &str) -> IResult<&str, Expr> {
    let (input, init) = factor(input)?;

    fold_many0(
        pair(sym("."), factor),
        move || init.clone(),
        |acc, (_, next_term)| Expr::App(Box::new(acc), Box::new(next_term)),
    )(input)
}

fn expr(input: &str) -> IResult<&str, Expr> {
    let (input, init) = term(input)?;

    fold_many0(
        pair(sym("+"), term),
        move || init.clone(),
        |acc, (_, next_term)| Expr::Add(Box::new(acc), Box::new(next_term)),
    )(input)
}

fn main_parser(input: &str) -> IResult<&str, Expr> {
    terminated(expr, multispace0)(input)
}

fn eval(expr: &Expr) -> String {
    match expr {
        Expr::Var(c) => c.to_string(),
        Expr::Par(e) => format!("({})", eval(e)),
        Expr::Add(e1, e2) => format!("{} + {}", eval(e1), eval(e2)),
        Expr::App(e1, e2) => format!("{} |> {}", eval(e2), eval(e1)),
    }
}

fn main() {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let function = input_line.trim_matches('\n').to_string();
    // let function = "(a + g)";
    match main_parser(&function) {
        Ok((_, expr)) => {
            // dbg!(&expr);
            println!("{}", eval(&expr));
        }
        Err(err) => println!("{:?}", err),
    }
}
