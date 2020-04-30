#[macro_use]
extern crate lazy_static;

use clap::{Arg, App};

mod lexer;
mod parser;
mod evaluator;
mod syntax;
mod utils;
mod tests;

use evaluator::evaluator::Evaluator;
use evaluator::interpreter::Interpreter;

use syntax::parse_tree::{ChainableExpression, ExpressionChain, ExpressionStart, FunctionApplication, Variable};
use crate::utils::{LocationContext, FileLocation};

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
    
    let mut interpreter = Interpreter::new();

    if let Err(e) = interpreter.interpret_file(src_file, &vec!["program".into()]) {
        panic!("unable to interpret code: {}", e);
    }

    interpreter.visit_chainable_expression(&main_func());
}

fn main_func() -> ChainableExpression {
    ChainableExpression {
        location: LocationContext {
            file: "main".to_string(),
            start_location: FileLocation { line: 0, col: 0 },
            end_location: FileLocation { line: 0, col: 0 }
        },
        start: ExpressionStart::VariableNode(
            Variable::Name(
                ( "main".into(), LocationContext {
                    file: "main".to_string(),
                    start_location: FileLocation { line: 0, col: 0 },
                    end_location: FileLocation { line: 0, col: 0 },
                })
            )
        ),
        chained: vec![
            ExpressionChain::FunctionApplicationNode(
                FunctionApplication{
                    location: LocationContext {
                        file: "main".to_string(),
                        start_location: FileLocation { line: 0, col: 0 },
                        end_location: FileLocation { line: 0, col: 0 },
                    },
                    args: vec![]
                }
            )
        ],
    }
}

