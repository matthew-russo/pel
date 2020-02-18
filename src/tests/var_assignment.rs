#[cfg(test)]
mod VarAssignmentTests {
    use crate::tests::runner::run_code;

    // #[test]
    fn var_assignment() {
        let code = r#"
            func main() {
                let x: int := 1;
            }
        "#;

        run_code(code.into()); 
    }

    // #[test]
    fn var_assignment_assertion() {
        let code = r#"
            func main() {
                let x: int := 1;
                assert(x).is(3);
            }
        "#;

        run_code(code.into()); 
    }

    // #[test]
    fn var_assignment_expression_assertion() {
        let code = r#"
            func main() {
                let x: int := 1 + 2;
                assert(x).is(3);
            }
        "#;

        run_code(code.into()); 
    }
}
