use crate::Compiler;

pub(crate) fn run_code(code: String) {
    let mut compiler = Compiler::new();
    if let Err(e) = compiler.interpret_code(code, &vec!["main".into()]) {
        panic!("{}", e);
    }
}
