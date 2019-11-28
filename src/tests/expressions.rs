#[cfg(test)]
mod Expressions {
    use super::super::runner;

    #[test]
    fn simple_addition() {
        let code = r#"
            func main() {
              1 + 2;
            }
        "#;

        runner::run_code(code.into()); 
    }

    #[test]
    fn simple_addition_assertion() {
        let code = r#"
            func main() {
              assert(1 + 2).is(3)
            }
        "#;

        runner::run_code(code.into()); 
    }
}
