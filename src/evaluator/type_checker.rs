struct TypeCheckError {
    message: String,
}

impl TypeCheckError {
    pub fn new(message: String) -> Self {
        Self {
            message,
        }
    }
}

impl std::error::Error for TypeError {

}

type TypeCheckResult = Result<(), TypeCheckError>;

impl TypeCheckResult {
    pub fn unknown_type() -> {
        let message = format!("type_id has not been set");
        return Err(TypeCheckError::new(message));
    }
}

struct TypeChecker {

}

impl Evaluator for TypeChecker {
    fn visit_program(&mut self, program: &Program) {

    }

    fn visit_declaration(&mut self, declaration: &Declaration) {

    }

    fn visit_enum_declaration(&mut self, enum_decl: &EnumDeclaration) {

    }

    fn visit_object_declaration(&mut self, obj_decl_parse: &parse_tree::ObjectDeclaration) {
        let obj_decl_id = SymbolId::generate();

        // TODO -> something with type params. I think at this point we just need to create
        // SymbolIds for them so we can reference them later

        // since types are first class, the type of a field is an expression so we need to
        // evaluate this expression now, validate it resolves to a type that exists,
        // and store that type_id
        let obj_decl_parse
            .fields
            .iter()
            .map(|(name, ty_expr)| {
                let ty_id = interpreter.evaluate(ty_expr);
                if !symbol_table.is_type(ty_id) {
                    panic!("fields need to resolve to types, but got {}", ty_id);
                }

                (name.clone(), ty_id)
            })
            .collect()

        // check that method signatures have `self` param
        // check that types in method signatures exist
        syntax::ObjDecl {
            id: obj_decl_id,
            type_params,
            fields,
            methods: Vec::new(),
            functions: Vec::new(),
        }
    }

    fn visit_contract_declaration(&mut self, contract_decl: &ContractDeclaration) {

    }

    fn visit_implementation_declaration(&mut self, impl_decl: &ImplementationDeclaration) {

    }

    fn visit_variant_declaration(&mut self, variant_decl: &VariantDeclaration) {

    }

    fn visit_function_declaration(&mut self, function_decl: &FunctionDeclaration) {

    // TODO ->  ensure that the function name doesn't already exist in the current env? it can
    // shadow declarations in parent env
    // TODO ->  ensure that the types referenced exist
    // TODO ->  ensure that the body's return type matche the declared return type
    }

    fn visit_typed_variable_declaration(&mut self, typed_var_decl: &TypedVariableDeclaration) {

    }

    fn visit_block_body(&mut self, block_body: &BlockBody) {
        // the type of a block body is either the type of its `return` statement(s) or Unit
        
        // if a block body has more than one `return` statement, all types must be the same
    }

    fn visit_statement(&mut self, statment: &Statement) {
        use syntax::Statement::*;
        match statement {
            VariableAssignmentNode(_) => Symbol::unit(),
            ExpressionNode(_) => Symbol::unit(),
            ReturnNode(_) => {
                // todo -> get type of return expr    
            },

        }
    }

    fn visit_variable_assignment(&mut self, var_assignment: &VariableAssignment) {

    }

    fn visit_return(&mut self, return_stmt: &Return) {

    }

    fn visit_expression(&mut self, expr: &Expression) {

    }

    fn visit_chainable_expression(&mut self, chainable_expr: &ChainableExpression) {

    }

    fn visit_conditional(&mut self, conditional: &Conditional) {

    }

    fn visit_match(&mut self, match_node: &Match) {

    }

    fn visit_loop(&mut self, loop_node: &Loop) {

    }

    fn visit_field_access(&mut self, field_access: &FieldAccess) {

    }

    fn visit_module_access(&mut self, mod_access: &ModuleAccess) {

    }

    fn visit_object_initialization(&mut self, obj_init_node: &ObjectInitialization) -> TypeCheckResult {
        let obj_decl_id = if let Some(type_id) = obj_init_node.type_id {
            type_id
        } else {
            return TypeCheckResult::unknown_type();
        }

        let obj_decl = symbol_table.load(obj_decl_id);

        for (type_arg_name, type_arg_id) in obj_decl.type_arguments.iter() {
            let type_arg = symbol_table.load(type_arg_id);
            if !type_arg.is_type_variable() {
                let message = format!("type argument {} to object {} is not of TypeVariable variant", type_arg_name, obj_decl_id);
                return TypeCheckResult::error(message);
            }
            
            if type_arg.is_unbound_type_variable() {
                let message = format!("type argument {} to object {} is unbound", type_arg_name, obj_decl_id);
                return TypeCheckResult::error(message);
            }
        }

        for field in obj_decl.fields.iter() {
            if !obj_init_node.fields.contains_key(field.name) {
                let message = format!("field {} is missing from initialization of object {}", field.name, obj_decl_id);
                return TypeCheckResult::error(message);
            }
        }

        for field_name in obj_init_node.fields.keys() {
            if !obj_decl_node.fields.contains_key(field_name) {
                let message = format!("uncrecognized field {} is the initialization of object {}", field_name, obj_decl_id);
                return TypeCheckResult::error(message);
            }
        }
        
        // TODO -> check that the type of all fields match
        for (field_name, field_type_id) in obj_decl.fields {
            let field_expr = obj_init_node.fields.get(&field_name);
            if field_type_id != field_expr.type_id {
                let message = format!("expected field {} to be of type {} but got type {}", field_name, field_type_id, field_expr.type_id);
                return TypeCheckResult::error(message);
            }
        }

        TypeCheckResult::ok()
    }

    // need to ensure that the types of args match the expected types of the params
    fn visit_function_application(&mut self, func_app_node: &FunctionApplication) {

    }

    fn visit_type_application(&mut self, type_app_node: &TypeApplication) {

    }

    fn visit_binary_operation(&mut self, bin_op: &BinaryOperation) {

    }

    fn visit_unary_operation(&mut self, unary_op: &UnaryOperation) {

    }

    fn visit_lambda(&mut self, lambda: &Lambda) {

    }
}
