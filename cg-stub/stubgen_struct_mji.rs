struct StubGen {
    pub commands: Vec<Cmd>,
    pub statement: String,
}

pub struct Var {
    pub name: String,
    pub t: T,
    pub input_comment: String,
}

pub enum T {
    Int,
    Float,
    Long,
    Bool,
    Word { max_length: usize },
    String { max_length: usize },
}

pub enum Cmd {
    Read(Var),
    Loop { max_count: String, inner_cmd: Box<Cmd> },
    LoopLine { object: String, vars: Vec<Var> },
    // output_text is the text associated by the OUTPUT statement
    Write { text: String, output_text: String },
}

trait GenerateStub {
    fn before(&mut self);
    fn statement(&mut self, statement: &str);
    fn cmd(&mut self, cmd: Cmd);
    fn after(&mut self);
}

struct Python {
    indent_level: usize
}
impl Python {
    fn new() -> Self {
        Self {
            indent_level: 0
        }
    }
}

impl GenerateStub for Python {
    fn before(&mut self) {
        println!("import sys\n\n\ndef main():");
        self.indent_level += 4;
    }

    fn statement(&mut self, statement: &str) {
        for line in statement.lines() {
            println!("{:indent$}# {}", "", line, indent=self.indent_level);
        }
    }

    fn cmd(&mut self, cmd: Cmd) {
        match cmd {
            Cmd::Read(var) => {
                println!("{:indent$}{} = input()  # {}", "", var.name, var.input_comment, indent=self.indent_level)
            }
            // ... write code for each Cmd here ...
            _ => todo!()
        }
    }

    fn after(&mut self) {
        println!("\nif __name__ == \"__main__\": main()")
    }
}

fn parse_stubgen() -> StubGen {
    StubGen {
        commands: vec![
            Cmd::Read(
                Var {
                    name: String::from("n"),
                    t: T::Int,
                    input_comment: String::from("number of bugs")
                }
            )
        ],
        statement: String::from("code some stuff")
    }
}

fn main() {
    let stub = parse_stubgen();
    let mut lang = Python::new();

    lang.before();
    lang.statement(&stub.statement);
    for cmd in stub.commands {
        lang.cmd(cmd);
    }
    lang.after();
}
