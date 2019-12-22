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
    let compiler = Compiler::from_file(src_file.into());
    if let Err(e) = compiler.interpret() {
        panic!("unable to interpret code: {}", e);
    }
}

struct Compiler {
    src: String,
}

impl Compiler {
    fn from_file(src_file: String) -> Self {
        let src = fs::read_to_string(src_file).unwrap();

        Self {
            src
        }
    }

    fn interpret(&self) -> Result<(), Box<dyn std::error::Error>> {
        let tokens = Lexer::new(self.src.clone()).lex()?;
        let program = Parser::new(tokens).parse()?;
        let mut interpreter = Interpreter::new();
        interpreter.visit_program(&program);
        interpreter.visit_chainable_expression(&main_func());
        Ok(())
    }
    
    fn compile(&self) -> Result<(), Box<dyn std::error::Error>> {
        panic!("codegen compilation unimplemented");
    }
}

fn main_func() -> ChainableExpression {
    ChainableExpression {
        start: ExpressionStart::VariableNode(Variable::Name("main".into())),
        chained: vec![ExpressionChain::FunctionApplicationNode(FunctionApplication{args: vec![]})],
    }
}

