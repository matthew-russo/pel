use crate::Interpreter;

pub(crate) fn run_code(code: String) {
    let mut interpreter = Interpreter::new();
    if let Err(e) = interpreter.interpret_code(code, &vec!["main".into()]) {
        panic!("{}", e);
    }
}
