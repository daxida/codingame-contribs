use std::io;
use std::io::Read;
use regex::Regex;


pub struct StubGen {
    pub commands: Vec<Cmd>,
    pub statement: String,
}

impl StubGen {
    fn new() -> Self {
        Self {
            commands: Vec::new(),
            statement: String::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: String,
    pub t: T,
    pub input_comment: String,
}

impl Var {
    fn new(name: String, t: T) -> Var {
        Var { name, t, input_comment: String::new() }
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

fn apply_case_vars(vars: &Vec<Var>, case_fun: fn(&str) -> String) -> Vec<Var> {
    vars.iter().map(|var| var.apply_case_var(case_fun)).collect()
}

#[derive(Debug, Clone)]
pub enum T {
    Int,
    Float,
    Long,
    Bool,
    Word { max_length: usize },
    String { max_length: usize },
}

#[derive(Debug, Clone)]
pub enum Cmd {
    Read(Vec<Var>),
    Loop { max_count: String, inner_cmd: Box<Cmd> },
    LoopLine { object: String, vars: Vec<Var> },
    // output_text is the text associated by the OUTPUT statement
    Write { text: String, output_text: String },
}

impl Cmd {
    fn apply_case_cmd(&self, case_fun: fn(&str) -> String) -> Cmd {
        match self {
            Cmd::Read(vars) => {
                Cmd::Read(apply_case_vars(vars, case_fun))
            },
            Cmd::Loop { max_count, inner_cmd } => {
                let transformed_inner = inner_cmd.apply_case_cmd(case_fun);
                Cmd::Loop {
                    max_count: case_fun(max_count),
                    inner_cmd: Box::new(transformed_inner),
                }
            },
            Cmd::LoopLine { object, vars } => {
                Cmd::LoopLine {
                    object: case_fun(object),
                    vars: apply_case_vars(vars, case_fun),
                }
            },
            other => other.clone(),
        }
    }
}

fn apply_case_cmds(cmds: &Vec<Cmd>, case_fun: fn(&str) -> String) -> Vec<Cmd> {
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
    fn interpolate_write(&self, msg: &String) -> String {
        format!("{}(\"{}\")", self.write, msg)
    }

    fn interpolate_forloop(&self, loop_var: &char, iter: String ) -> String {
        format!("{} {} {} {}:", self.forloop_forword, loop_var, self.forloop_inword, iter)
    }
}

struct Lang<'a> {
    // Stub independent (this should be configurable)
    imports: &'a str,
    add_imports: bool,
    auto_generated_message: &'a str,
    add_auto_generated_message: bool,
    write_message: &'a str,

    // Config
    space_indentation: usize,
    indent: String,
    case_fun: fn(&str) -> String,

    // Coments
    single_line_comment: String,
    // multi_line_comment: String,

    keywords: Keywords,
}

const ALPHABET: [char; 18] = ['i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'];

impl<'a> Lang<'a> {
    fn new(
        imports: &'a str,
        add_imports: bool,
        auto_generated_message: &'a str,
        add_auto_generated_message: bool,
        write_message: &'a str,
        space_indentation: usize, 
        case_fun: fn(&str) -> String, 
        single_line_comment: String, 
        keywords: Keywords
    ) -> Self {
        Self {
            imports: imports,
            add_imports: add_imports,
            auto_generated_message: auto_generated_message,
            add_auto_generated_message: add_auto_generated_message,
            write_message: write_message,
            space_indentation, 
            indent: " ".repeat(space_indentation), 
            case_fun, 
            single_line_comment, 
            keywords,
        }
    }

    fn generate_stub(&self, stub: StubGen) -> String {
        // TODO: use self.write_message

        let cmds = apply_case_cmds(&stub.commands, self.case_fun);
        let statement = stub.statement;
    
        let mut buffer = String::new();

        // Libraries if any
        if self.add_imports {
            buffer.push_str(self.imports)
        }
        // Autogenerated message if any
        if self.add_auto_generated_message {
            let message = self.auto_generated_message
                .split("\n")
                .map(|line| format!("{} {}", self.single_line_comment, line))
                .collect::<Vec<String>>()
                .join("\n")
                + "\n\n";
            buffer.push_str(message.as_str())
        }  
        // Statement
        if !statement.is_empty() {
            let commented_statement = self.generate_statement(statement);
            buffer.push_str(format!("{}\n\n", &commented_statement).as_str());
        }

        // Main function if any
    
        for cmd in cmds {
            let arg = &self.keywords.input;
            let line = self.cmd_to_s(&cmd, 0, arg.clone());
            buffer.push_str(line.as_str());
        }
    
        buffer.trim_end().to_string()
    }

    fn generate_statement(&self, statement: String) -> String {
        statement
            .split("\n")
            .map(|line| 
                format!("{} {}", self.single_line_comment, line)
            )
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn cmd_to_s(&self, cmd: &Cmd, loop_count_level: usize, arg: String) -> String {
        match cmd {
            Cmd::Read(read_ts) => self.read_to_s(&read_ts, arg),
            Cmd::Loop { max_count, inner_cmd } => 
                self.loop_to_s(&max_count, &inner_cmd, loop_count_level, arg),
            Cmd::LoopLine { object, vars } => 
                self.loopline_to_s(&object, &vars, loop_count_level),
            Cmd::Write { text, output_text } => self.write_to_s(&text, output_text),
        }
    }

    fn read_to_s(&self, read_vars: &Vec<Var>, arg: String) -> String {
        let out = match read_vars.as_slice() {
            [ ] => panic!("Empty read_ts in to_pyc_read"),
            [single_var] => {
                // Case 1: one single read_t 
                eprintln!("READ case 1: {:?}", single_var);

                self.var_to_assignment(single_var, arg, true)
            },
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
 
                    let var_names = read_vars
                        .iter()
                        .map(|var| var.name.clone())
                        .collect::<Vec<String>>()
                        .join(", ");
                    let keyword = self.type_to_keyword(&fst_var.t);
                    let expr = match keyword.as_str() {
                        // x, y = input().split()
                        "" => "input().split()".to_string(),
                        // x, y = [int(i) for i in input().split()]
                        _ => format!("[{}(i) for i in input().split()]", keyword)
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
                        .map(|(idx, var)| {
                            self.var_to_assignment(var, format!("{}[{}]", inputs, idx), true)
                        })
                        .collect::<Vec<_>>()
                        .join("\n");
                    format!("{}\n{}", split_input, inner)
                }
            }
        };

        format!("{}\n", out)
    }

    fn loop_to_s(&self, max_count: &String, inner_cmd: &Box<Cmd>, loop_count_level: usize, arg: String) -> String {
        let loop_var = ALPHABET[loop_count_level];
        let iter = format!("range({})", max_count);
        let forloop = self.keywords.interpolate_forloop(&loop_var, iter);
        match inner_cmd.as_ref() {
            Cmd::Read(read_cmds) => {
                // 2 cases, either only one command or more than one
                match read_cmds.as_slice() {
                    [ ] => panic!("Empty vector in loop at command py"),
                    [single_var] => {
                        let assignment = self.var_to_assignment(single_var, arg, true);
                        let indented_inner_pyt = indent(assignment, &self.indent);
                        format!("{}\n{}\n", forloop, indented_inner_pyt)
                    },
                    _ => {
                        let arg = "inputs[0]".to_string();
                        let inner_cmd_py = self.cmd_to_s(inner_cmd.as_ref(), loop_count_level + 1, arg);
                        let indented_inner_cmd_py = indent(inner_cmd_py, &self.indent);
                        format!("{}\n{}\n", forloop, indented_inner_cmd_py)
                    }
                }
            },
            Cmd::Loop { .. } => {
                let inner_loop_py = self.cmd_to_s(inner_cmd.as_ref(), loop_count_level + 1, arg);
                let indented_inner_loop_py = indent(inner_loop_py, &self.indent);
                format!("{}\n{}\n", forloop, indented_inner_loop_py)
            },
            Cmd::Write { .. } => {
                let inner_write_py = self.cmd_to_s(inner_cmd.as_ref(), loop_count_level, arg);
                let indented_inner_write_py = indent(inner_write_py, &self.indent);
                format!("{}\n{}\n", forloop, indented_inner_write_py)
            },
            Cmd::LoopLine { .. } => panic!("Loopline inside loop")
        }
    }

    fn loopline_to_s(&self, object: &String, vars: &Vec<Var>, loop_count_level: usize) -> String {
        let loop_var = ALPHABET[loop_count_level];
                
        match vars.as_slice() {
            [ ] => panic!("Empty vector at loopline_to_s"),
            [single_var] => {
                let iter = format!("input().split()");
                let forloop = self.keywords.interpolate_forloop(&loop_var, iter);
                let assignment = self.var_to_assignment(single_var, loop_var.to_string(), true);
                let indented_assingment = indent(assignment, &self.indent);
                format!("{}\n{}\n", forloop, indented_assingment)
            },
            // Loopline with multiple arguments
            _ => {
                // INPUT comments: they come AT THE TOP.
                let mut input_comment = self.input_comment_from_vars(vars);
                if !input_comment.is_empty() {
                    input_comment = format!("{}{}", self.indent, input_comment)
                }

                let iter = format!("range({})", object);
                let forloop = self.keywords.interpolate_forloop(&loop_var, iter);
                let split_input = format!("inputs = input().split()");
                let length = vars.len();
                let inner_split: String = vars
                    .iter()
                    .enumerate()
                    .map(|(idx, var)| {
                        let arg = if idx == 0 {
                            format!("inputs[{}*{}]", length, loop_var)
                        } else {
                            format!("inputs[{}*{}+{}]", length, loop_var, idx)
                        };
                        self.var_to_assignment(var, arg, false)
                    })
                    .collect::<Vec<String>>()
                    .join("\n");
                let indented_inner_split = indent(inner_split, &self.indent);
                format!(
                    "{}\n{}\n{}{}\n", 
                    split_input, 
                    forloop, 
                    input_comment, 
                    indented_inner_split
                )
            }
        }
    }

    fn write_to_s(&self, msg: &String, output_text: &String) -> String {
        let commented_output_text = if output_text.is_empty() {
            // do nothing
            "".to_string()
        } else {
            output_text
                .split('\n')
                .map(|line| 
                    format!("{} {}", self.single_line_comment, line)
                )
                .collect::<Vec<_>>()
                .join("\n")
                + "\n"
        };

        // Deals with write word1\nword2
        let write_py = msg
            .split('\n')
            .map(|line| {
                let msg = line.trim_end().to_string();
                let write_py_line = self.keywords.interpolate_write(&msg);
                format!("{}", &write_py_line)
            })
            .collect::<Vec<String>>()
            .join("\n");
            
        format!("{}{}\n", commented_output_text, write_py)
    }

    // This can't be hardcorded

    fn type_to_keyword(&self, var_type: &T) -> String {
        match var_type {
            T::Int { .. } => "int",
            T::Float { .. } => "float",
            T::Long { .. } => "int",
            T::Bool { .. } => "",
            T::Word { .. } => "",
            T::String { .. } => "",
        }.to_string()
    }

    fn var_to_assignment(&self, var: &Var, arg: String, comments_right: bool) -> String {
        let assignment = match var.t {
            T::Int => format!("{} = int({})", var.name, arg),
            T::Float => format!("{} = float({})", var.name, arg),
            T::Long => format!("{} = int({})", var.name, arg),
            T::Bool => format!("{} = {}", var.name, arg),
            T::Word { .. } => format!("{} = {}", var.name, arg),
            T::String { .. } => format!("{} = {}", var.name, arg),
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

    fn input_comment_from_vars(&self, read_vars: &Vec<Var>) -> String {
        let input_comment = read_vars
            .iter()
            .map(|var| {
                if var.input_comment.is_empty() {
                    "".to_string()
                } else {
                    format!(
                        "{} {}: {}",
                        self.single_line_comment,
                        var.name,
                        var.input_comment
                    )
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

fn indent(text: String, indent: &String) -> String {
    text
    .trim_end()
    .split("\n")
    .map(|line| format!("{}{}", indent, line))
    .collect::<Vec<String>>()
    .join("\n")
}

pub fn parse_generator_stub(generator: String) -> StubGen {
    // Hacky -- for the parser to work
    let generator = generator
        .replace("\n", " \n ")
        .replace("\n  \n", "\n \n");
    dbg!(&generator);
    let mut stream = generator.split(" ").peekable();
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
        Self { stream, input_banned_vars: Vec::new() }
    }

    fn parse(&mut self) -> StubGen {
        let mut stub_gen = StubGen::new();

        while let Some(token) = self.stream.next() {
            match token {
                "read" => stub_gen.commands.push(self.parse_read()),
                "write" => stub_gen.commands.push(self.parse_write()),
                "loop" => stub_gen.commands.push(self.parse_loop()),
                "loopline" => stub_gen.commands.push(self.parse_loopline()),
                "OUTPUT" =>  self.parse_output_comment(&mut stub_gen), 
                "INPUT" => self.parse_input_comment(&mut stub_gen), 
                "STATEMENT" => stub_gen.statement = self.parse_statement(),
                "\n" | "" => continue,
                thing => panic!("Unknown token stub generator: {}", thing),
            };
        }

        stub_gen
    }

    fn parse_read(&mut self) -> Cmd {
        Cmd::Read(self.parse_variable_list())
    }

    fn parse_message(&mut self) -> String {
        let mut output: Vec<String> = Vec::new();

        while let Some(token) = self.stream.next() {
            let next_token = match token { 
                "\n" => {
                    match self.stream.next() {
                        Some("\n") | None => break,
                        Some(str) => format!("\n{}", str),
                    }
                }
                other => String::from(other),
            };
            output.push(next_token);
        };

        // Hacky - The replace is due to the "replace hacks" at generator
        // so that ["you", "\ndown"] -> you\ndown
        output
        .join(" ")
        .replace(" \n", "\n")
        .as_str().trim().to_string()
    }

    fn parse_write(&mut self) -> Cmd {
        Cmd::Write { text: self.parse_message(), output_text: String::new() }
    }

    fn parse_statement(&mut self) -> String {
        // Ignore current line
        while let Some(token) = self.stream.next() {
            match token { "\n" => break, _ => () };
        }

        self.parse_message()
    }

    fn parse_output_comment(&mut self, stub_gen: &mut StubGen) {
        let output_text_parsed = self.parse_statement();
        self.recursively_backward_update_output(stub_gen, output_text_parsed)
    }

    fn recursively_backward_update_output(&mut self, stub_gen: &mut StubGen, output_text_parsed: String) {
        let mut new_stub_cmds: Vec<Cmd> = Vec::new();
        for previous_cmd in &stub_gen.commands {
            let new_cmd = match previous_cmd {
                Cmd::Write { text, ref output_text } if output_text.is_empty() => {
                    Cmd::Write { 
                        text: text.to_string(), 
                        output_text: output_text_parsed.clone() 
                    }
                },
                Cmd::Loop { max_count, inner_cmd } => {
                    // Recur
                    let mut inner_stub = StubGen::new(); // temporary wrapper
                    inner_stub.commands.push(*inner_cmd.clone());
                    self.recursively_backward_update_output(&mut inner_stub, output_text_parsed.clone());
                    Cmd::Loop {
                        max_count: max_count.clone(),
                        inner_cmd: Box::new(inner_stub.commands.pop().unwrap()),
                    }
                },
                _ => previous_cmd.clone(),
            };

            new_stub_cmds.push(new_cmd);
        }

        stub_gen.commands = new_stub_cmds;
    }

    fn parse_input_comment(&mut self, stub_gen: &mut StubGen) {
        let input_comment_parsed = self.parse_statement();
        // Here extract the significant pairs (Do with a hash eventually)
        let extracted: Vec<(String, String)> = input_comment_parsed
            .lines()
            .filter(|line| line.contains(":"))
            .map(|line| {
                let parts: Vec<String> = line.split(":").map(|s| s.to_string()).collect();
                // Assuming there are at least two parts
                let (first, second) = match parts.split_first() {
                    Some((first, rest)) => (first.to_string(), rest.join(":")),
                    None => panic!("No way since I filtered??"),
                };
                (first, second)
            })
            .collect();

        let seen_read_vars = self.recursively_backward_update_input(stub_gen, extracted.clone());

        // Update banned words --
        // A word is banned if at one point we had an INPUT assignation for a Var
        // but that Var was not declared yet.
        // before in the stub generator. Example:
        //   INPUT
        //   x: a label
        //
        //   read x:int
        //
        //   INPUT
        //   x: another label
        //
        // This would not give, as could be expected
        //   x = int(input())
        //   x = int(input())  # another label
        // But
        //   x = int(input())
        //   x = int(input())
        
        // NOTE: Can't do this recursively, it would wrongly ban words if it had to recur
        let banned_words: Vec<(String, String)> = extracted
            .iter()
            .filter(|(kwd, _)| {
                let is_banned = seen_read_vars.iter().all(|var| var.name != *kwd);
                if is_banned {
                    self.input_banned_vars.push(kwd.to_string());
                }
                !is_banned
            })
            .cloned()
            .collect();
    }

    fn recursively_backward_update_input(&mut self, stub_gen: &mut StubGen, extracted: Vec<(String, String)>) -> Vec<Var> {
        let mut seen_read_vars: Vec<Var> = Vec::new();
        let mut new_stub_cmds: Vec<Cmd> = Vec::new();
        for previous_cmd in &stub_gen.commands {
            let new_cmd = match previous_cmd {
                Cmd::Read(vars) => {
                    // No recur: iterate the vars
                    let new_vars = self.update_vars(vars, &mut seen_read_vars, &extracted);
                    Cmd::Read(new_vars)
                },
                Cmd::Loop { max_count, inner_cmd } => {
                    // Recur
                    let mut inner_stub = StubGen::new(); // temporary wrapper
                    inner_stub.commands.push(*inner_cmd.clone());
                    self.recursively_backward_update_input(&mut inner_stub, extracted.clone());
                    Cmd::Loop {
                        max_count: max_count.clone(),
                        inner_cmd: Box::new(inner_stub.commands.pop().unwrap()),
                    }
                },
                Cmd::LoopLine { object,  vars } => {
                    // No recur: iterate the vars
                    let new_vars = self.update_vars(vars, &mut seen_read_vars, &extracted);
                    Cmd::LoopLine {
                        object: object.clone(),
                        vars: new_vars,
                    }
                }
                _ => previous_cmd.clone(),
            };
            new_stub_cmds.push(new_cmd);
        }
 
        stub_gen.commands = new_stub_cmds;

        seen_read_vars
    }

    fn update_vars(&self, vars: &Vec<Var>, seen_read_vars: &mut Vec<Var>, extracted: &Vec<(String, String)>) -> Vec<Var> {
        vars
            .iter()
            .map(|var| {
                // If the var.name is banned, skip.
                if self.input_banned_vars.contains(&var.name) {
                    eprintln!("Exited because {} is banned", &var.name);
                    return var.clone();
                }
                seen_read_vars.push(var.clone());
                // Don't override previous commented assignments
                if !var.input_comment.is_empty() {
                    return var.clone();
                }
                // Find a suiting keyword - comment pair
                let input_comment = extracted
                    .iter()
                    .filter(|(kwd, _)| var.name == *kwd)
                    .cloned()
                    .next() // Get the first matching keyword or None
                    .map(|(_, ic)| ic.trim().to_string())
                    .unwrap_or(var.input_comment.clone());
    
                Var {
                    name: var.name.clone(),
                    t: var.t.clone(),
                    input_comment,
                }
            })
            .collect()
    }

    fn parse_loop(&mut self) -> Cmd {
        let count = match self.stream.next() {
            Some("\n") | None => panic!("Loop stub not provided with loop count"),
            Some(other) => String::from(other),
        };
        let command = self.parse_read_or_write(); // nor really, also loop
        Cmd::Loop { max_count: count, inner_cmd: command }
    }

    fn parse_loopline(&mut self) -> Cmd {
        let object = match self.stream.next() {
            Some("\n") | None => panic!("Loopline stub not provided with identifier to loop through"),
            Some(other) => String::from(other),
        };
        let vars = self.parse_variable_list();
        Cmd::LoopLine { object, vars }
    }

    fn parse_variable_list(&mut self) -> Vec<Var> {
        let mut vars = Vec::new();
        while let Some(token) = self.stream.next() {
            let var: Var = match token {
                _ if String::from(token).contains(":") => {
                    Self::parse_variable(token)
                },
                "\n" => break,
                // Doesn't work -> "..., found {unexp} while searching ..."
                unexp => panic!(
                    format!("Error in stub generator, found {} while searching for stub variables", unexp)
                ),
            };
            vars.push(var);
        };
        vars
    }

    fn parse_variable(token: &str) -> Var {
        let mut iter = token.split(":");
        let name = String::from(iter.next().unwrap());
        let var_type = iter.next().expect("Error in stub generator: missing type");
        let length_regex = Regex::new(r"(word|string)\((\d+)\)").unwrap();
        let length_captures = length_regex.captures(var_type);
        match var_type {
            "int" => Var::new(name, T::Int),
            "float" => Var::new(name, T::Float),
            "long" => Var::new(name, T::Long),
            "bool" => Var::new(name, T::Bool),
            _ => {
                let caps = length_captures
                    .expect(format!(
                        "Failed to parse variable type for token: {}", &token
                    ).as_str());
                let new_type = caps.get(1).unwrap().as_str();
                let max_length: usize = caps.get(2).unwrap().as_str().parse().unwrap();
                match new_type {
                    "word" => Var::new(name, T::Word{max_length}),
                    "string" => Var::new(name, T::String{max_length}),
                    _ => panic!("Unexpected error")
                }
            }
        }
    }

    fn parse_read_or_write(&mut self) -> Box<Cmd> {
        let cmd = match self.stream.next() {
            Some("loop") => self.parse_loop(), // added
            Some("read") => self.parse_read(),
            Some("write") => self.parse_write(),
            Some(thing) => panic!("Error parsing loop command in stub generator, got: {}", thing),
            None => panic!("Loop with no arguments in stub generator"),
        };
        Box::new(cmd)
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

    let py_keywords = Keywords { 
        input: "input()".to_string(),
        forloop_forword: "for".to_string(),
        forloop_inword: "in".to_string(),
        write: "print".to_string(),
    };

    let py = Lang::new(
        "import sys\nimport math\n\n",
        false,
        "Auto-generated code below aims at helping you parse\nthe standard input according to the problem statement.",
        false,
        "Write an answer using print\nTo debug: print(\"Debug messages...\", file=sys.stderr, flush=True)",
        4, 
        camel_to_snake, 
        "#".to_string(), 
        py_keywords
    );
    let py_stub = py.generate_stub(stub_generator);

    print!("{}", py_stub)
}