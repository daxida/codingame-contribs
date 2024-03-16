#![allow(clippy::while_let_on_iterator)]

use std::io;
use std::io::Read;

use regex::Regex;

#[derive(Default)]
pub struct Stub {
    pub commands: Vec<Cmd>,
    pub statement: String,
}

#[derive(Debug, Clone)]
pub enum T {
    Int,
    Float,
    Long,
    Bool,
    Word { max_length: String },
    String { max_length: String },
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: String,
    pub t: T,
    pub input_comment: String,
}

impl Var {
    fn new(name: String, t: T) -> Var {
        Var {
            name,
            t,
            input_comment: String::new(),
        }
    }

    fn same_t(&self, other: &Var) -> bool {
        std::mem::discriminant(&self.t) == std::mem::discriminant(&other.t)
    }

    fn apply_case_var(&self, case_fun: fn(&str) -> String) -> Var {
        Var {
            name: case_fun(self.name.as_str()),
            t: self.t.clone(),
            input_comment: self.input_comment.clone(),
        }
    }
}

fn apply_case_vars(vars: &[Var], case_fun: fn(&str) -> String) -> Vec<Var> {
    vars.iter().map(|var| var.apply_case_var(case_fun)).collect()
}

#[derive(Clone, Debug)]
pub enum LengthType {
    Number,
    Variable,
}

impl<'a> From<&'a str> for LengthType {
    fn from(value: &'a str) -> Self {
        match value.parse::<usize>() {
            Ok(_) => Self::Number,
            Err(_) => Self::Variable,
        }
    }
}

// JoinTerms
#[derive(Clone, Debug)]
pub enum JoinTermType {
    Literal,
    Variable,
}

#[derive(Clone, Debug)]
pub struct JoinTerm {
    pub name: String,
    pub term_type: JoinTermType,
}

impl JoinTerm {
    pub fn new(name: String, term_type: JoinTermType) -> Self {
        Self { name, term_type }
    }
}

#[derive(Debug, Clone)]
pub enum Cmd {
    Read(Vec<Var>),
    Loop { count_var: String, inner_cmd: Box<Cmd> },
    LoopLine { count_var: String, vars: Vec<Var> },
    Write { text: String, output_comment: String },
    WriteJoin(Vec<JoinTerm>),
}

impl Cmd {
    fn apply_case_cmd(&self, case_fun: fn(&str) -> String) -> Cmd {
        match self {
            Cmd::Read(vars) => Cmd::Read(apply_case_vars(vars, case_fun)),
            Cmd::Loop { count_var, inner_cmd } => Cmd::Loop {
                count_var: case_fun(count_var),
                inner_cmd: Box::new(inner_cmd.apply_case_cmd(case_fun)),
            },
            Cmd::LoopLine { count_var, vars } => Cmd::LoopLine {
                count_var: case_fun(count_var),
                vars: apply_case_vars(vars, case_fun),
            },
            Cmd::WriteJoin(join_terms) => Cmd::WriteJoin(
                join_terms
                    .iter()
                    .map(|term| match term.term_type {
                        // Only apply casing to variables and not literals
                        JoinTermType::Variable => JoinTerm {
                            name: case_fun(&term.name),
                            term_type: term.term_type.clone(),
                        },
                        JoinTermType::Literal => term.clone(),
                    })
                    .collect(),
            ),
            other => other.clone(),
        }
    }
}

fn apply_case_cmds(cmds: &[Cmd], case_fun: fn(&str) -> String) -> Vec<Cmd> {
    cmds.iter().map(|cmd| cmd.apply_case_cmd(case_fun)).collect()
}

pub struct Keywords {
    pub input: String,
    // There are not even forloops in some languages (ruby)
    forloop_forword: String,
    forloop_inword: String,
    write: String,
}

impl Keywords {
    fn interpolate_write(&self, msg: &str) -> String {
        format!("{}(\"{}\")", self.write, msg)
    }

    fn interpolate_forloop(&self, loop_var: &str, iter: String) -> String {
        format!("{} {} {} {}:", self.forloop_forword, loop_var, self.forloop_inword, iter)
    }
}

struct Lang<'a> {
    // Stub independent
    imports: &'a str,
    auto_generated_message: &'a str,
    _write_message: &'a str,

    // Config
    _space_indentation: usize,
    indent: String,
    case_fun: fn(&str) -> String,

    // Coments
    single_line_comment: String,
    // multi_line_comment: String,
    keywords: Keywords,
}

const ALPHABET: [&str; 18] = [
    "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
];

impl<'a> Lang<'a> {
    fn new(
        imports: &'a str,
        auto_generated_message: &'a str,
        _write_message: &'a str,
        _space_indentation: usize,
        case_fun: fn(&str) -> String,
        single_line_comment: String,
        keywords: Keywords,
    ) -> Self {
        Self {
            imports,
            auto_generated_message,
            _write_message,
            _space_indentation,
            indent: " ".repeat(_space_indentation),
            case_fun,
            single_line_comment,
            keywords,
        }
    }

    fn generate_stub(&self, stub: Stub, add_imports: bool, add_autogenerated_msg: bool) -> String {
        // TODO: use self.write_message
        let mut buffer = String::new();

        // Libraries if any
        if add_imports {
            buffer.push_str(self.imports)
        }
        // Autogenerated message if any
        if add_autogenerated_msg {
            let message = self
                .auto_generated_message
                .split('\n')
                .map(|line| format!("{} {}", self.single_line_comment, line))
                .collect::<Vec<String>>()
                .join("\n")
                + "\n\n";
            buffer.push_str(&message)
        }
        // Statement
        if !stub.statement.is_empty() {
            let commented_statement = self.generate_statement(&stub.statement);
            buffer.push_str(&format!("{}\n\n", &commented_statement));
        }
        // Main function if any
        let cmds = apply_case_cmds(&stub.commands, self.case_fun);
        for cmd in cmds {
            let arg = &self.keywords.input;
            let line = self.cmd_to_s(&cmd, 0, arg);
            buffer.push_str(&line);
        }

        buffer.trim_end().to_string()
    }

    fn generate_statement(&self, statement: &str) -> String {
        statement
            .split('\n')
            .map(|line| format!("{} {}", self.single_line_comment, line))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn cmd_to_s(&self, cmd: &Cmd, loop_count_level: usize, arg: &str) -> String {
        match cmd {
            Cmd::Read(read_ts) => self.read_to_s(read_ts, arg),
            Cmd::Loop { count_var, inner_cmd } => self.loop_to_s(count_var, inner_cmd, loop_count_level, arg),
            Cmd::LoopLine { count_var, vars } => self.loopline_to_s(count_var, vars, loop_count_level),
            Cmd::Write { text, output_comment } => self.write_to_s(text, output_comment),
            Cmd::WriteJoin(join_terms) => self.write_join_to_s(join_terms),
        }
    }

    fn read_to_s(&self, read_vars: &[Var], arg: &str) -> String {
        let out = match read_vars {
            [] => panic!("Empty read_ts in to_pyc_read"),
            [single_var] => {
                // Case 1: one single read_t
                eprintln!("READ case 1: {:?}", single_var);

                self.var_to_assignment(single_var, arg, true)
            }
            _ => {
                // Case 2.1: all the types are the same and we group the split
                //           INPUT comments come at the top
                // Case 2.2: the types are different and we need to separate them
                //           INPUT comments are dealt at var_to_assignment
                let fst_var = &read_vars[0];
                if read_vars.iter().all(|t| t.same_t(fst_var)) {
                    eprintln!("READ case 2.1: {:?}", read_vars);

                    // INPUT comments: they come AT THE TOP of the unique assignment
                    let input_comment = self.input_comment_from_vars(read_vars);

                    let var_names =
                        read_vars.iter().map(|var| var.name.clone()).collect::<Vec<String>>().join(", ");
                    let keyword = Self::type_to_keyword(&fst_var.t);
                    let expr = match keyword {
                        // x, y = input().split()
                        "" => "input().split()".to_string(),
                        // x, y = [int(i) for i in input().split()]
                        _ => format!("[{}(i) for i in input().split()]", keyword),
                    };
                    format!("{}{} = {}", input_comment, var_names, expr)
                } else {
                    eprintln!("READ case 2.2: {:?}", read_vars);

                    // INPUT comments: they come AT THE RIGHT of each assignment

                    let inputs = "inputs";
                    let split_input = format!("{} = input().split()", inputs);
                    let inner: String = read_vars
                        .iter()
                        .enumerate()
                        .map(|(idx, var)| self.var_to_assignment(var, &format!("{}[{}]", inputs, idx), true))
                        .collect::<Vec<_>>()
                        .join("\n");
                    format!("{}\n{}", split_input, inner)
                }
            }
        };

        format!("{}\n", out)
    }

    fn loop_to_s(&self, count_var: &str, inner_cmd: &Cmd, loop_count_level: usize, arg: &str) -> String {
        let loop_var = ALPHABET[loop_count_level];
        let iter = format!("range({})", count_var);
        let forloop = self.keywords.interpolate_forloop(loop_var, iter);
        match inner_cmd {
            Cmd::Read(read_cmds) => {
                // 2 cases, either only one command or more than one
                match read_cmds.as_slice() {
                    [] => panic!("Empty vector in loop at command py"),
                    [single_var] => {
                        let assignment = self.var_to_assignment(single_var, arg, true);
                        let indented_inner_pyt = indent(&assignment, &self.indent);
                        format!("{}\n{}\n", forloop, indented_inner_pyt)
                    }
                    _ => {
                        let arg = "inputs[0]".to_string();
                        let inner_cmd_py = self.cmd_to_s(inner_cmd, loop_count_level + 1, &arg);
                        let indented_inner_cmd_py = indent(&inner_cmd_py, &self.indent);
                        format!("{}\n{}\n", forloop, indented_inner_cmd_py)
                    }
                }
            }
            Cmd::Loop { .. } => {
                let inner_loop_py = self.cmd_to_s(inner_cmd, loop_count_level + 1, arg);
                let indented_inner_loop_py = indent(&inner_loop_py, &self.indent);
                format!("{}\n{}\n", forloop, indented_inner_loop_py)
            }
            Cmd::Write { .. } => {
                let inner_write_py = self.cmd_to_s(inner_cmd, loop_count_level, arg);
                let indented_inner_write_py = indent(&inner_write_py, &self.indent);
                format!("{}\n{}\n", forloop, indented_inner_write_py)
            }
            Cmd::LoopLine { .. } => {
                let inner_loopline_py = self.cmd_to_s(inner_cmd, loop_count_level + 1, arg);
                let indented_inner_loopline_py = indent(&inner_loopline_py, &self.indent);
                format!("{}\n{}\n", forloop, indented_inner_loopline_py)
            }
            Cmd::WriteJoin(_) => todo!(),
        }
    }

    fn loopline_to_s(&self, count_var: &str, vars: &[Var], loop_count_level: usize) -> String {
        let loop_var = ALPHABET[loop_count_level];

        match vars {
            [] => panic!("Empty vector at loopline_to_s"),
            [single_var] => match single_var.t {
                // Special case when it's a word
                T::Word { .. } => {
                    // TODO: what happens if this gets an INPUT comment???
                    let iter = "input().split()".to_string();
                    let forloop = self.keywords.interpolate_forloop(&single_var.name, iter);
                    format!("{}\n{}pass\n", forloop, &self.indent)
                }
                _ => {
                    // Comment comes AT THE TOP even if single var
                    let iter = "input().split()".to_string();
                    let forloop = self.keywords.interpolate_forloop(loop_var, iter);
                    let assignment = self.var_to_assignment(single_var, loop_var, false);
                    let indented_assingment = indent(&assignment, &self.indent);
                    let comment = if single_var.input_comment.is_empty() {
                        "".to_string()
                    } else {
                        format!(
                            "{}{} {}: {}\n",
                            &self.indent, self.single_line_comment, single_var.name, single_var.input_comment
                        )
                    };
                    format!("{}\n{}{}\n", forloop, comment, indented_assingment)
                }
            },
            // Loopline with multiple arguments
            _ => {
                // INPUT comments: they come AT THE TOP.
                let mut input_comment = self.input_comment_from_vars(vars);
                if !input_comment.is_empty() {
                    input_comment = format!("{}{}", self.indent, input_comment)
                }

                let iter = format!("range({})", count_var);
                let forloop = self.keywords.interpolate_forloop(loop_var, iter);
                let split_input = "inputs = input().split()".to_string();
                let length = vars.len();
                let inner_split = vars
                    .iter()
                    .enumerate()
                    .map(|(idx, var)| {
                        let arg = if idx == 0 {
                            format!("inputs[{}*{}]", length, loop_var)
                        } else {
                            format!("inputs[{}*{}+{}]", length, loop_var, idx)
                        };
                        self.var_to_assignment(var, &arg, false)
                    })
                    .collect::<Vec<String>>()
                    .join("\n");
                let indented_inner_split = indent(&inner_split, &self.indent);
                format!("{}\n{}\n{}{}\n", split_input, forloop, input_comment, indented_inner_split)
            }
        }
    }

    fn write_to_s(&self, msg: &str, output_comment: &str) -> String {
        let output_comment = if output_comment.is_empty() {
            // do nothing
            "".to_string()
        } else {
            output_comment
                .split('\n')
                .map(|line| format!("{} {}", self.single_line_comment, line))
                .collect::<Vec<_>>()
                .join("\n")
                + "\n"
        };

        // Deals with write word1\nword2
        let write_py = msg
            .split('\n')
            .map(|line| self.keywords.interpolate_write(line.trim()).to_string())
            .collect::<Vec<_>>()
            .join("\n");

        format!("{}{}\n", output_comment, write_py)
    }

    fn write_join_to_s(&self, join_terms: &[JoinTerm]) -> String {
        // TODO: support comments

        let write_py: String = join_terms
            .iter()
            .map(|t| match t.term_type {
                JoinTermType::Variable => format!("str({})", &t.name),
                JoinTermType::Literal => format!("\"{}\"", &t.name),
            })
            .collect::<Vec<_>>()
            .join(" + \" \" + ")
            // Hack to merge string literals
            .replace("\" + \"", "");

        format!("print({})\n", write_py)
    }

    // This can't be hardcorded (it should be a Language parameter)
    #[rustfmt::skip]
    fn type_to_keyword(var_type: &T) -> &str {
        match var_type {
            T::Int { .. }    => "int",
            T::Float { .. }  => "float",
            T::Long { .. }   => "int",
            T::Bool { .. }   => "",
            T::Word { .. }   => "",
            T::String { .. } => "",
        }
    }

    fn var_to_assignment(&self, var: &Var, arg: &str, comments_right: bool) -> String {
        let assignment = match Self::type_to_keyword(&var.t) {
            "" => format!("{} = {}", var.name, arg),
            kw => format!("{} = {}({})", var.name, kw, arg),
        };

        if comments_right {
            let input_comment = if var.input_comment.is_empty() {
                "".to_string()
            } else {
                format!("  {} {}", self.single_line_comment, var.input_comment)
            };

            format!("{}{}", assignment, input_comment)
        } else {
            assignment
        }
    }

    fn input_comment_from_vars(&self, read_vars: &[Var]) -> String {
        let input_comment = read_vars
            .iter()
            .map(|var| {
                if var.input_comment.is_empty() {
                    "".to_string()
                } else {
                    format!("{} {}: {}", self.single_line_comment, var.name, var.input_comment)
                }
            })
            .collect::<Vec<String>>()
            .join("\n")
            .trim()
            .to_string();

        if !input_comment.is_empty() {
            input_comment + "\n"
        } else {
            input_comment
        }
    }
}

fn indent(text: &str, indent: &str) -> String {
    text.trim_end()
        .split('\n')
        .map(|line| format!("{}{}", indent, line))
        .collect::<Vec<String>>()
        .join("\n")
}

pub fn parse_generator_stub(generator: String) -> Stub {
    // Hacky -- for the parser to work
    let generator = generator.replace('\n', " \n ").replace("\n  \n", "\n \n");
    let stream = generator.split(' ').peekable();
    Parser::new(stream).parse()
}

struct Parser<StreamType: Iterator> {
    stream: StreamType,
    input_banned_vars: Vec<String>,
}

impl<'a, I> Parser<I>
where
    I: Iterator<Item = &'a str>,
{
    fn new(stream: I) -> Self {
        Self {
            stream,
            input_banned_vars: Vec::new(),
        }
    }

    #[rustfmt::skip]
    fn parse(&mut self) -> Stub {
        let mut stub = Stub::default();

        while let Some(token) = self.stream.next() {
            match token {
                "read"      => stub.commands.push(self.parse_read()),
                "write"     => stub.commands.push(self.parse_write()),
                "loop"      => stub.commands.push(self.parse_loop()),
                "loopline"  => stub.commands.push(self.parse_loopline()),
                "OUTPUT"    => self.parse_output_comment(&mut stub.commands),
                "INPUT"     => self.parse_input_comment(&mut stub.commands),
                "STATEMENT" => stub.statement = self.parse_statement(),
                "\n" | ""   => continue,
                thing => panic!("Unknown token stub generator: {}", thing),
            };
        }

        stub
    }

    fn parse_read(&mut self) -> Cmd {
        Cmd::Read(self.parse_variable_list())
    }

    fn parse_write(&mut self) -> Cmd {
        let mut write_text: Vec<String> = Vec::new();
        let mut first_line = true;

        while let Some(token) = self.stream.next() {
            let next_token = match token {
                "\n" => {
                    first_line = false;
                    match self.stream.next() {
                        Some("\n") | None => break,
                        Some(str) => format!("\n{}", str),
                    }
                }
                // Note that: write•join()•rest⏎, with NOTHING inside the parens,
                // gets parsed as a write and not as a write_join
                join if join.starts_with("join(") && !join.starts_with("join()") && first_line => {
                    return self.parse_write_join(join)
                }
                other => String::from(other),
            };

            write_text.push(next_token);
        }

        Cmd::Write {
            text: write_text.join(" "),
            output_comment: String::new(),
        }
    }

    // Note that there is no nesting here
    fn parse_write_join(&mut self, start: &str) -> Cmd {
        let mut raw_vec = vec![start];

        while let Some(token) = self.stream.next() {
            match token {
                "\n" => {
                    if start.contains(')') {
                        // write•join("a")⏎ is a valid join
                        let terms = Self::parse_write_join_inner(start);
                        return Cmd::WriteJoin(terms)
                    } else {
                        // write•join(⏎ gets parsed as a raw_string
                        return Cmd::Write {
                            text: raw_vec.join(" "),
                            output_comment: String::new(),
                        }
                    }
                }
                term => {
                    raw_vec.push(term);
                    if term.contains(')') {
                        break;
                    }
                }
            }
        }

        self.skip_to_next_line();

        let raw_string = raw_vec.join(" ");
        let terms = Self::parse_write_join_inner(&raw_string);

        // write•join("hi",,,•"Jim")⏎ should be rendered as a Write Cmd
        // (I guess the original parser fails a previous command due to consecutive commas)
        // We have no way to know if they are multiple commas until we call parse_write_join.
        // NOTE: maybe this fails at write•join("hi"•,•,•,•"Jim")?
        if terms.iter().any(|jt| jt.name.is_empty()) {
            return Cmd::Write {
                text: raw_string,
                output_comment: String::new(),
            }
        }

        Cmd::WriteJoin(terms)
    }

    fn parse_write_join_inner(inner: &str) -> Vec<JoinTerm> {
        let terms_finder = Regex::new(r"join\(([^)]*)\)").unwrap();
        let terms_string_captures = terms_finder.captures(inner);
        let terms_string = terms_string_captures.unwrap().get(1).unwrap().as_str();
        let term_splitter = Regex::new(r",\s*").unwrap();
        let literal_matcher = Regex::new("\\\"([^)]+)\\\"").unwrap();

        term_splitter
            .split(terms_string)
            .map(|term_str| {
                if let Some(mtch) = literal_matcher.captures(term_str) {
                    JoinTerm::new(mtch.get(1).unwrap().as_str().to_owned(), JoinTermType::Literal)
                } else {
                    // let var_type = Self::find_variable_type( .. );
                    JoinTerm::new(term_str.to_owned(), JoinTermType::Variable)
                }
            })
            .collect()
    }

    // Recursively try to find the previous read Command for the current Variable
    // This is relevant because if the variable is a str, then we don't need to wrap it into str()
    // F.ex:
    //   read var:string(10)
    //   write join(var, "b")
    // Should return print(var + " b") and not print(str(var) + " b")
    fn _find_variable_type() {
        todo!()
    }

    fn parse_text_block(&mut self) -> String {
        let mut text_block: Vec<String> = Vec::new();

        while let Some(token) = self.stream.next() {
            let next_token = match token {
                "\n" => match self.stream.next() {
                    Some("\n") | None => break,
                    Some(str) => format!("\n{}", str),
                },
                other => String::from(other),
            };
            text_block.push(next_token);
        }

        // Hacky - The replace is due to the "replace hacks" at generator
        // so that ["you", "\ndown"] -> you\ndown ...
        // while trimming spaces both sides of each line
        text_block
            .join(" ")
            .replace(" \n", "\n")
            .split('\n')
            .map(|line| line.trim())
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn parse_statement(&mut self) -> String {
        self.skip_to_next_line();
        self.parse_text_block()
    }

    fn skip_to_next_line(&mut self) {
        while let Some(token) = self.stream.next() {
            if token == "\n" {
                break
            }
        }
    }

    fn parse_output_comment(&mut self, previous_commands: &mut [Cmd]) {
        let output_comment = self.parse_statement();
        for cmd in previous_commands {
            Self::update_cmd_with_output_comment(cmd, &output_comment)
        }
    }

    fn update_cmd_with_output_comment(cmd: &mut Cmd, new_comment: &str) {
        match cmd {
            Cmd::Write {
                text: _,
                ref mut output_comment,
            } if output_comment.is_empty() => *output_comment = new_comment.to_string(),
            Cmd::Loop {
                count_var: _,
                ref mut inner_cmd,
            } => {
                Self::update_cmd_with_output_comment(inner_cmd, new_comment);
            }
            _ => (),
        }
    }

    // Update banned words --
    // A word is banned if at one point we had an INPUT assignation for a Var
    // but that Var was not declared yet in the stub generator. Example:
    //   INPUT
    //   x: a label
    //
    //   read x:int
    //
    //   INPUT
    //   x: another label
    //
    // This would not give, as could be expected
    //   x = int(input())  # another label
    // But
    //   x = int(input())
    fn parse_input_comment(&mut self, previous_commands: &mut [Cmd]) {
        // Here extract the significant pairs (Do with a hash eventually)
        let input_statement = self.parse_statement();
        let input_comment_pairs: Vec<(&str, &str)> = input_statement
            .lines()
            .filter(|line| line.contains(':'))
            .map(|line| {
                if let Some((var, rest)) = line.split_once(':') {
                    (var, rest.trim())
                } else {
                    panic!("Impossible since the list was filtered??");
                }
            })
            .collect();

        for (ic_var, ic_comment) in input_comment_pairs {
            if self.input_banned_vars.contains(&String::from(ic_var)) {
                continue;
            }

            // If it didnt find an assignment to add the comment to, we ban ic_var
            if !previous_commands
                .iter_mut()
                .any(|cmd| Self::update_cmd_with_input_comment(cmd, ic_var, ic_comment))
            {
                self.input_banned_vars.push(String::from(ic_var));
            }
        }
    }

    // Returns true in case a the current comment was assigned.
    // Otherwise, returns false so that we can ban that variable.
    fn update_cmd_with_input_comment(cmd: &mut Cmd, ic_var: &str, ic_comment: &str) -> bool {
        match cmd {
            Cmd::Read(vars) => {
                for var in vars.iter_mut() {
                    if var.name == *ic_var && var.input_comment.is_empty() {
                        var.input_comment = String::from(ic_comment);
                        return true;
                    }
                }
            }
            Cmd::Loop {
                count_var: _,
                ref mut inner_cmd,
            } => {
                return Self::update_cmd_with_input_comment(inner_cmd, ic_var, ic_comment);
            }
            Cmd::LoopLine {
                count_var: _,
                ref mut vars,
            } => {
                for var in vars.iter_mut() {
                    if var.name == *ic_var && var.input_comment.is_empty() {
                        var.input_comment = String::from(ic_comment);
                        return true;
                    }
                }
            }
            _ => (),
        }

        false
    }

    fn parse_loop(&mut self) -> Cmd {
        match self.next_past_newline() {
            Some("\n") => panic!("Could not find count identifier for loop"),
            None => panic!("Unexpected end of input: Loop stub not provided with loop count"),
            Some(other) => Cmd::Loop {
                count_var: String::from(other),
                inner_cmd: Box::new(self.parse_loopable()),
            },
        }
    }

    fn parse_loopable(&mut self) -> Cmd {
        match self.next_past_newline() {
            Some("\n") => panic!("Loop not provided with command"),
            Some("read") => self.parse_read(),
            Some("write") => self.parse_write(),
            Some("loopline") => self.parse_loopline(),
            Some("loop") => self.parse_loop(),
            Some(thing) => panic!("Error parsing loop command in stub generator, got: {}", thing),
            None => panic!("Unexpected end of input, expecting command to loop through"),
        }
    }

    fn parse_loopline(&mut self) -> Cmd {
        match self.next_past_newline() {
            Some("\n") => panic!("Could not find count identifier for loopline"),
            None => panic!("Unexpected end of input: Loopline stub not provided with count identifier"),
            Some(other) => Cmd::LoopLine {
                count_var: other.to_string(),
                vars: self.parse_variable_list(),
            },
        }
    }

    fn parse_variable_list(&mut self) -> Vec<Var> {
        let mut vars = Vec::new();
        for token in self.stream.by_ref() {
            let var = match token {
                _ if String::from(token).contains(':') => Self::parse_variable(token),
                "\n" => break,
                other => {
                    panic!("Error in stub generator, found {} while searching for stub variables", other)
                }
            };
            vars.push(var);
        }
        vars
    }

    fn parse_variable(token: &str) -> Var {
        let mut iter = token.split(':');
        let name = String::from(iter.next().unwrap());
        let var_type = iter.next().expect("Error in stub generator: missing type");
        match var_type {
            "int" => Var::new(name, T::Int),
            "float" => Var::new(name, T::Float),
            "long" => Var::new(name, T::Long),
            "bool" => Var::new(name, T::Bool),
            _ => {
                let length_regex = Regex::new(r"(word|string)\((\w+)\)").unwrap();
                let length_captures = length_regex.captures(var_type);
                let caps = length_captures
                    .unwrap_or_else(|| panic!("Failed to parse variable type for token: {}", &token));
                let new_type = caps.get(1).unwrap().as_str();
                let length = caps.get(2).unwrap().as_str();
                let max_length = String::from(length);
                match new_type {
                    "word" => Var::new(name, T::Word { max_length }),
                    "string" => Var::new(name, T::String { max_length }),
                    _ => panic!("Unexpected error"),
                }
            }
        }
    }

    fn next_past_newline(&mut self) -> Option<&'a str> {
        match self.stream.next() {
            Some("\n") => self.stream.next(),
            Some("") => self.next_past_newline(),
            token => token,
        }
    }
}

fn camel_to_snake(input: &str) -> String {
    let mut result = String::new();
    let mut prev_char_was_upper = false;

    for c in input.chars() {
        if c.is_uppercase() {
            if !result.is_empty() && !prev_char_was_upper {
                result.push('_');
            }
            result.push(c.to_ascii_lowercase());
            prev_char_was_upper = true;
        } else {
            result.push(c);
            prev_char_was_upper = false;
        }
    }

    result
}

fn main() {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let mut code = String::new();
    io::stdin().read_to_string(&mut code).unwrap();
    let stub_generator = parse_generator_stub(code);

    let single_line_comment = "#".to_string();
    let py_keywords = Keywords {
        input: "input()".to_string(),
        forloop_forword: "for".to_string(),
        forloop_inword: "in".to_string(),
        write: "print".to_string(),
    };
    let py = Lang::new(
        "import sys\nimport math\n\n",
        "Auto-generated code below aims at helping you parse\nthe standard input according to the problem statement.",
        "Write an answer using print\nTo debug: print(\"Debug messages...\", file=sys.stderr, flush=True)",
        4, 
        camel_to_snake, 
        single_line_comment, 
        py_keywords
    );
    let py_stub = py.generate_stub(stub_generator, false, false);

    print!("{}", py_stub)
}
