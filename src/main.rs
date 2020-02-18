#[macro_use]
extern crate lazy_static;

use clap::{Arg, App};

mod lexer;
mod parser;
mod evaluator;
mod syntax;
mod utils;
mod tests;

use std::fs;
use std::path::Path;

use lexer::lexer::Lexer;
use parser::parser::Parser;
use evaluator::interpreter::Interpreter;
use evaluator::evaluator::Evaluator;

use syntax::parse_tree::{ChainableExpression, ExpressionChain, ExpressionStart, FunctionApplication, Variable};

fn main() {
    let src_file_arg_name = "src_file";

    let app = App::new("pel")
        .version("0.1")
        .author("Matthew Russo <matthew@matthewclayrusso.com>")
        .about("Compiler and Interpreter for the PEL language")
        .arg(Arg::with_name(src_file_arg_name)
             .help("Sets the input file to use")
             .required(true)
             .index(1));

    let matches = app.get_matches();
    let src_file = matches.value_of(src_file_arg_name).unwrap();
    let mut compiler = Compiler::new();
    if let Err(e) = compiler.interpret_file(src_file, &vec!["program".into()]) {
        panic!("unable to interpret code: {}", e);
    }
}

struct Compiler {
    lexer: Lexer,
    parser: Parser,
    interpreter: Interpreter,
}

impl Compiler {
    fn new() -> Self {
        let lexer = Lexer::new();
        let parser = Parser::new();
        let interpreter = Interpreter::new();

        let mut compiler = Self {
            lexer,
            parser,
            interpreter,
        };

        compiler.load_std_lib();

        compiler
    }

    fn load_std_lib(&mut self) {
        if let Err(e) = self.load_dir("stdlib", vec!["pel".into()]) {
            println!("failed to load stdlib: {:?}", e);
        }
    }

    fn load_dir(&mut self, dir: &str, current: Vec<String>) -> Result<(), Box<dyn std::error::Error>> {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                let mut new_current = current.clone();
                new_current.push(path.file_name().unwrap().to_os_string().into_string().unwrap());
                self.load_dir(path.as_path().to_str().unwrap(), new_current)?;
            } else {
                self.interpret_file(path, &current)?;
            }
        }
    
        Ok(())
    }
    
    fn interpret_file<P: AsRef<Path>>(&mut self, src_file: P, module_chain: &Vec<String>) -> Result<(), Box<dyn std::error::Error>> {
        let src = fs::read_to_string(src_file)?;
        self.interpret_code(src, module_chain)
    }

    fn interpret_code(&mut self, code: String, module_chain: &Vec<String>) -> Result<(), Box<dyn std::error::Error>> {
        let tokens = self.lexer.lex(code)?;
        let program = self.parser.parse(tokens)?;
        self.interpreter.visit_program(&program, module_chain);
        self.interpreter.visit_chainable_expression(&main_func());
        Ok(())
    }
    
    fn compile(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        panic!("codegen compilation unimplemented");
    }
}

fn main_func() -> ChainableExpression {
    ChainableExpression {
        start: ExpressionStart::VariableNode(Variable::Name("main".into())),
        chained: vec![ExpressionChain::FunctionApplicationNode(FunctionApplication{args: vec![]})],
    }
}

