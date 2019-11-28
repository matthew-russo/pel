#[cfg(test)]
mod VarAssignmentTests {
    #[test]
    fn var_assignment() {
        let code = r#"
            func main() {
                let x: Integer := 1 + 2;
            }
        "#;

        runner::run_code(code.into()); 
    }

    #[test]
    fn var_assignment_assertion() {
        let code = r#"
            func main() {
                let x: Integer := 1 + 2;
                assert(x).is(3)
            }
        "#;

        runner::run_code(code.into()); 
    }
}
