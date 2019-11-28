#[cfg(test)]
mod Objects {
    #[test]
    fn obj_declaration() {
        let code = r#"
            func main() {
             
            }

            object User {
              fields {
                first_name: String,
                last_name: String,
              }
            }
        "#;

        runner::run_code(code.into());
    }

    #[test]
    fn obj_declaration_generic() {
        let code = r#"
            func main() {

            }

            object Wrapper<T> {
                fields {
                    internal: T,
                }
            }
        "#;
    }

    #[test]
    fn obj_creation() {
        let code = r#"
            func main() {
              let user := User {
                first_name: "Matthew",
                last_name: "Russo",
              };
            }
            
            object User {
              fields {
                first_name: String,
                last_name: String,
              }
            }
        "#;

        runner::run_code(code.into()); 
    }

    #[test]
    fn obj_creation_generic() {
        let code = r#"
            func main() {
              let user := Wrapper<String> {
                internal: "wrapped",
              };
            }
            
            object Wrapper<T> {
              fields {
                internal: T,
              }
            }
        "#;

        runner::run_code(code.into()); 
    }

    #[test]
    fn obj_field_access() {
        let code = r#"
            func main() {
              let user := User {
                first_name: "Matthew",
                last_name: "Russo",
              };

              assert(user.first_name).is("Matthew");
              assert(user.last_name).is("Russo");
            }
            
            object User {
              fields {
                first_name: String,
                last_name: String,
              }
            }
        "#;

        runner::run_code(code.into()); 
    }

    #[test]
    fn obj_field_access_generic() {
        let code = r#"
            func main() {
              let wrapped := Wrapper<String> {
                internal: "wrapped",
              };

              assert(wrapper.internal).is("wrapped");
            }
            
            object Wrapper<T> {
              fields {
                wrapped: T,
              }
            }
        "#;

        runner::run_code(code.into()); 
    }

    fn obj_methods() {
        let code = r#"
            func main() {
              let user := User {
                first_name: "Matthew",
                last_name: "Russo",
              };

              assert(user.full_name).is("Matthew Russo");
            }
            
            object User {
              fields {
                first_name: String,
                last_name: String,
              }
            
              methods {
                public func full_name(self) -> String {
                    return self.first_name
                        .concat(" ")
                        .concat(self.last_name);
                }
              }
            }
        "#;

        runner::run_code(code.into()); 
    }
}
