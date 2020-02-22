#[cfg(test)]
mod Expressions {
    use crate::tests::runner::run_code;

    #[test]
    fn simple_int_addition() {
        let code = r#"
            func main() {
              1 + 2;
            }
        "#;

        run_code(code.into()); 
    }

    #[test]
    fn simple_int_addition_assertion() {
        let code = r#"
            use pel::lang::assert;

            func main() {
              assert<<int>>(1 + 2).is(3);
            }
        "#;

        run_code(code.into()); 
    }
}
