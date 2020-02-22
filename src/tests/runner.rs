use crate::Interpreter;
use crate::Evaluator;
use crate::main_func;

pub(crate) fn run_code(code: String) {
    let mut interpreter = Interpreter::new();
    if let Err(e) = interpreter.interpret_code(code, &vec!["main".into()]) {
        panic!("{}", e);
    }

    interpreter.visit_chainable_expression(&main_func());
}
