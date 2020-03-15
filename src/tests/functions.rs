#[cfg(test)]
mod FunctionTests {
    use crate::tests::runner::run_code;

    #[test]
    fn function_declaration_no_args() {
        let code = r#"
            func main() { }

            func add_one_and_two() -> int; {
              return 1 + 2;
            }
        "#;

        run_code(code.into());
    }

    #[test]
    fn function_declaration_args() {
        let code = r#"
            func main() { }

            func add(one: int, two: int) -> int; {
              return one + two;
            }
        "#;

        run_code(code.into());
    }

    #[test]
    fn function_declaration_generic_param() {
        let code = r#"
            use pel::lang::string;

            func main() { }

            func id<<T>>(input: T) -> String; {
              return "Hello";
            }
        "#;

        run_code(code.into());
    }

    #[test]
    fn function_declaration_generic_param_and_return() {
        let code = r#"
            func main() { }

            func id<<T>>(input: T) -> T; {
              return input;
            }
        "#;

        run_code(code.into());
    }

    #[test]
    fn function_declaration_multiple_generics() {
        let code = r#"
            func main() { }

            func pair_of<<A, B>>(left: A, right: B) -> (A, B); {
              return (left, right);
            }
        "#;

        run_code(code.into());
    }

    #[test]
    fn function_call_no_args() {
        let code = r#"
            use pel::lang::assert;

            func main() {
              assert<<int>>(add_one_and_two()).is(3);
            }
            
            func add_one_and_two() -> int; {
              return 1 + 2;
            }
        "#;

        run_code(code.into()); 
    }

    #[test]
    fn function_call_args() {
        let code = r#"
            use pel::lang::assert;

            func main() {
              assert<<int>>(add(1, 2)).is(3);
            }
            
            func add(one: int, two: int) -> int; {
              return one + two;
            }
        "#;

        run_code(code.into()); 
    }

    #[test]
    fn function_call_generics() {
        let code = r#"
            use pel::lang::assert;

            func main() {
                assert<<String>>("hello").is("hello");
                assert<<int>>(42).is(42);
            }

            func id<<T>>(input: T) -> T; {
              return input;
            }
        "#;

        run_code(code.into()); 
    }

    #[test]
    fn function_call_multiple_generics() {
        let code = r#"
            use pel::lang::assert;

            func main() {
              assert<<(String, int)>>(pair_of<<String, int>>("hello", 42).is(("hello", 42));
              assert<<(int, String)>>(pair_of<<int, String>>(42, "hello").is((42, "hello"));
            }
            
            func pair_of<<A, B>>(left: A, right: B) -> (A, B); {
              return (left, right);
            }
        "#;

        run_code(code.into()); 
    }
}
