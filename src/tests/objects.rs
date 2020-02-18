#[cfg(test)]
mod Objects {
    use crate::tests::runner::run_code;

    #[test]
    fn obj_declaration() {
        let code = r#"
            use pel::lang::string;

            func main() {
             
            }

            object User {
              fields {
                first_name: String,
                last_name: String,
              }
            }
        "#;

        run_code(code.into());
    }

    #[test]
    fn obj_declaration_generic() {
        let code = r#"
            func main() {

            }

            object Wrapper<<T>> {
                fields {
                    internal: T,
                }
            }
        "#;

        run_code(code.into());
    }

    #[test]
    fn obj_creation() {
        let code = r#"
            use pel::lang::string;

            func main() {
              let user: User := User {
                first_name: "Mason",
                last_name: "Hitchcock",
              };
            }
            
            object User {
              fields {
                first_name: String,
                last_name: String,
              }
            }
        "#;

        run_code(code.into()); 
    }

    #[test]
    fn obj_creation_generic() {
        let code = r#"
            use pel::lang::string;

            func main() {
              let wrapped_str: Wrapper<<String>> := Wrapper<<String>> {
                internal: "wrapped",
              };

              let wrapped_int: Wrapper<<int>> := Wrapper<<int>> {
                internal: 42,
              };
            }
            
            object Wrapper<<T>> {
              fields {
                internal: T,
              }
            }
        "#;

        run_code(code.into()); 
    }

    #[test]
    fn obj_methods() {
        let code = r#"
            use pel::lang::string;

            func main() {
              let user: User := User {
                first_name: "Mason",
                last_name: "Hitchcock",
              };

              assert(user.greeting()).is("hello");
            }
            
            object User {
              fields {
                first_name: String,
                last_name: String,
              }
            
              methods {
                public func greeting(self) -> String {
                    return "hello";
                }
              }
            }
        "#;

        run_code(code.into()); 
    }

    #[test]
    fn obj_field_access() {
        let code = r#"
            use pel::lang::string;

            func main() {
              let user: User := User {
                first_name: "Mason",
                last_name: "Hitchcock",
              };

              assert(user.first_name()).is("Mason Hitchcock");
            }
            
            object User {
              fields {
                first_name: String,
                last_name: String,
              }
            
              methods {
                public func first_name(self) -> String {
                    return self.first_name;
                }
              }
            }
        "#;

        run_code(code.into()); 
    }
}
