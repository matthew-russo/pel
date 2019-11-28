
#[cfg(test)]
mod FunctionTests {
    #[test]
    fn function_declaration_no_args() {
        let code = r#"
            func main() {

            }

            func add_one_and_two() -> Integer {
              return 1 + 2;
            }
        "#;

        runner::run_code(code.into());
    }

    #[test]
    fn function_declaration_args() {
        let code = r#"
            func main() {

            }

            func add(one: Integer, two: Integer) -> Integer {
              return one + two;
            }
        "#;

        runner::run_code(code.into());
    }

    #[test]
    fn function_declaration_generic_param() {
        let code = r#"
            func main() {

            }

            func id<T>(input: T) -> String {
              return "Hello";
            }
        "#;

        runner::run_code(code.into());
    }

    #[test]
    fn function_declaration_generic_param_and_return() {
        let code = r#"
            func main() {

            }

            func id<T>(input: T) -> T {
              return input;
            }
        "#;

        runner::run_code(code.into());
    }

    #[test]
    fn function_declaration_multiple_generics() {
        let code = r#"
            func main() {

            }

            func pair_of<A, B>(left: A, right: B) -> (A, B) {
              return (left, right);
            }
        "#;

        runner::run_code(code.into());
    }

    #[test]
    fn function_call_no_args() {
        let code = r#"
            func main() {
              add_one_and_two();
            }
            
            func add_one_and_two() -> Integer {
              return 1 + 2;
            }
        "#;

        runner::run_code(code.into()); 
    }

    #[test]
    fn function_call_no_args_assertion() {
        let code = r#"
            func main() {
              assert(add_one_and_two()).is(3);
            }
            
            func add_one_and_two() -> Integer {
              return 1 + 2;
            }
        "#;

        runner::run_code(code.into()); 
    }

    #[test]
    fn function_call_generics() {
        let code = r#"
            func main() {
                assert(id("hello")).is("hello");
                assert(id(42)).is(42);
            }

            func id<T>(input: T) -> T {
              return input;
            }
        "#;

        runner::run_code(code.into()); 
    }

    #[test]
    fn function_call_multiple_generics() {
        let code = r#"
            func main() {
              assert(pair_of("hello", 42).is(("hello", 42));
              assert(pair_of(42, "hello").is((42, "hello"));
            }
            
            func pair_of<A, B>(left: A, right: B) -> (A, B) {
              return (left, right);
            }
        "#;

        runner::run_code(code.into()); 
    }

    #[test]
    fn function_call_args() {
        let code = r#"
            func main() {
              add(1, 2);
            }
            
            func add(x: Integer, y: Integer) -> Integer {
              return x + y;
            }
        "#;

        runner::run_code(code.into()); 
    }

    #[test]
    fn function_call_args_assertion() {
        let code = r#"
            func main() {
              assert(add(1, 2)).is(3);
            }
            
            func add(x: Integer, y: Integer) -> Integer {
              return x + y;
            }
        "#;

        runner::run_code(code.into()); 
    }
}
