use crate::Compiler;

pub fn run_code(code: String) {
    let compiler = Compiler { src: code };
    if let Err(e) = compiler.interpret() {
        panic!("{}", e);
    }
}
