use crate::syntax::syntax::*;
use crate::evaluator::evaluator::{Evaluator, SymbolTable, SymbolId, Environment, Symbol, Function, Object, ObjectInstance, Enum, EnumInstance, NativeFunction, Callable};

use std::sync::{Arc, RwLock};
use std::ops::Deref;

//        -- environment --            ----- state -----
//      /                   \        /                   \
// name                       symbol                      value
// 
// Symbol Table -- Type Information
// +------+------+----+---------+
// | Name | Kind | ID | Address |
// +------+------+----+---------+
// 
// Object Table -- Inserted into during ObjectDeclarations
// +----+--------------------+
// | Id | List<FieldOffsets> |
// +----+--------------------+
// 
// Object Field Offsets
// +-----------+------+--------+
// | FieldName | Kind | Offset |
// +-----------+------+--------+
// 
// Function Table -- Inserted into during FunctionDeclaration
// +------+---------------+---------------+
// | Name | AddressOfBody | EnvironmentId |
// +------+---------------+---------------+

pub(crate) struct Interpreter {
    symbol_table: SymbolTable,
    global_env: Arc<RwLock<Environment>>,
    current_env: Arc<RwLock<Environment>>,
    last_local: Option<SymbolId>,
    exiting: bool,
    errors: Vec<String>,
}

const SELF_VAR_SYMBOL_NAME: &str = "self";
const SELF_TYPE_SYMBOL_NAME: &str = "Self";

impl Interpreter {
    pub fn new() -> Self {
        let mut symbol_table = SymbolTable::new();

        let global = Arc::new(RwLock::new(Environment::global(&mut symbol_table)));
        
        Self {
            symbol_table,
            global_env: global.clone(),
            current_env: global,
            last_local: None,
            exiting: false,
            errors: Vec::new(),
        }
    }

    pub fn emit_error(&mut self, error: String) {
        self.errors.push(error);
    }

    fn var_name(assignment_node: &VariableAssignment) -> Option<String> {
        let ce = if let Expression::ChainableExpressionNode(ce) = &assignment_node.target {
            ce
        } else {
            return None;
        };

        if !ce.chained.is_empty() {
            return None;
        }

        let var = if let ExpressionStart::VariableNode(var) = &ce.start {
            var
        } else {
            return None;
        };

        let var_name = match var {
            Variable::Name(n) => n.clone(),
            Variable::SelfVariable => SELF_VAR_SYMBOL_NAME.into(),
            Variable::SelfType => SELF_TYPE_SYMBOL_NAME.into(),
        };
        
        Some(var_name)
    }

    pub fn set_current_env(&mut self, env: Environment) {
        self.current_env = Arc::new(RwLock::new(env)); 
    }

    pub fn get_last_local(&mut self) -> SymbolId {
        self.last_local.take().unwrap()
    }
}

impl Evaluator for Interpreter {
    fn visit_program(&mut self, program: &Program) {
        for decl in program.declarations.iter() {
            self.visit_declaration(decl);
        }
    }

    fn visit_declaration(&mut self, decl: &Declaration) {
        use Declaration::*;

        match decl {
            EnumDeclarationNode(enum_decl) =>
                self.visit_enum_declaration(enum_decl),

            ContractDeclarationNode(contract_decl) =>
                self.visit_contract_declaration(contract_decl),

            ImplementationDeclarationNode(impl_decl) =>
                self.visit_implementation_declaration(impl_decl),

            ObjectDeclarationNode(obj_decl) =>
                self.visit_object_declaration(obj_decl),

            FunctionDeclarationNode(func_decl) =>
                self.visit_function_declaration(func_decl),
        }
    }

    fn visit_enum_declaration(&mut self, enum_decl: &EnumDeclaration) {
        panic!("unimplemented visit_enum_declaration");
        // let enum_symbol = Enum::from(enum_decl);
        // self.current_env.define(enum_decl.type_name, enum_symbol);
    }

    fn visit_object_declaration(&mut self, obj_decl: &ObjectDeclaration) {
        let mut local_env = Environment::from_parent(&self.current_env);
        for type_param_name in obj_decl.type_params.iter() {
            let symbol_id = self.symbol_table.new_symbol(Symbol::TypeVariable(None));
            local_env.define(type_param_name.clone(), symbol_id);
        }
        self.current_env = Arc::new(RwLock::new(local_env));

        // TODO -> eerily similar to Evaluator::Function::from_function_decl_syntax
        let type_arguments = obj_decl.type_params
            .iter()
            .map(|t| (t.clone(), SymbolTable::EMPTY_TYPE_VARIABLE_ID))
            .collect();

        let fields = obj_decl.fields.fields
            .iter()
            .map(|tvd| {
                self.visit_expression(&tvd.type_reference);
                // type check that last_local is a type/kind
                let evaluated = self.last_local.take().unwrap();
                (tvd.name.clone(), evaluated)
            })
            .collect();

        let functions = obj_decl.functions.functions
            .iter()
            .map(|fd| Function::from_function_decl_syntax(fd, &self.global_env))
            .collect();

        let methods = obj_decl.methods.methods
            .iter()
            .map(|fd| Function::from_function_decl_syntax(fd, &self.global_env))
            .collect();

        let obj = Object {
            type_arguments,
            fields,
            functions,
            methods,
        };

        let obj_sym = Symbol::Object(obj);

        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;

        let symbol_id = self.symbol_table.new_symbol(obj_sym);
        self.current_env
            .write()
            .unwrap()
            .define(obj_decl.type_name.clone(), symbol_id);
    }

    fn visit_contract_declaration(&mut self, contract_decl: &ContractDeclaration) {
        panic!("unimplemented visit_contract_declaration");
        // let contract_sym = Contract::from(contract_decl);
        // self.current_env.define(contract_decl.type_name, contract_sym);
    }

    fn visit_implementation_declaration(&mut self, impl_decl: &ImplementationDeclaration) {
        panic!("unimplemented visit_implementation_declaration");

    }

    fn visit_variants(&mut self, variants: &Variants) {
        panic!("unimplemented visit_variants");

    }

    fn visit_variant_declaration(&mut self, variant_decl: &VariantDeclaration) {
        panic!("unimplemented visit_variant_declaration");

    }

    fn visit_fields(&mut self, fields: &Fields) {
        panic!("unimplemented visit_fields");

    }

    fn visit_methods(&mut self, methods: &Methods) {
        panic!("unimplemented visit_methods");

    }

    fn visit_functions(&mut self, functions: &Functions) {
        panic!("unimplemented visit_functions");

    }

    fn visit_function_declaration(&mut self, func_decl: &FunctionDeclaration) {
        let name = &func_decl.signature.name;

        let already_exists = {
            self.current_env.read().unwrap().get_symbol_by_name(name).is_some()
        };
            
        if already_exists {
            // let message = format!("function {} already defined at {}", name,
            // sym.defined_at());
            self.emit_error("function already defined".into());
            return;
        }
        
        let func_sym = Symbol::Function(Function::from_function_decl_syntax(func_decl, &self.current_env));

        let symbol_id = self.symbol_table.new_symbol(func_sym);
        self.current_env
            .write()
            .unwrap()
            .define(name.clone(), symbol_id);
    }

    fn visit_typed_variable_declaration(&mut self, typed_var_decl: &TypedVariableDeclaration) {
        panic!("unimplemented typed_variable_declaration");
    }

    fn visit_block_body(&mut self, block_body: &BlockBody) {
        for stmt in block_body.statements.iter() {
            if self.exiting {
                self.exiting = false;
                return;
            }

            self.visit_statement(stmt);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        use Statement::*;

        match statement {
            VariableAssignmentNode(var_assignment) => {
                if let Some(var_name) = Self::var_name(var_assignment) {
                    let symbol_id = self.symbol_table.new_symbol(Symbol::Value(Value::IntegerValue(0)));

                    self.current_env
                        .write()
                        .unwrap()
                        // TODO -> This isn't right...
                        .define(var_name, symbol_id);
                }

                self.visit_expression(&var_assignment.target);
                let target_sym_id = self.last_local.take().unwrap();

                self.visit_expression(&var_assignment.target_type);
                let target_type_sym_id = self.last_local.take().unwrap();

                self.visit_expression(&var_assignment.value);
                let value_sym_id = self.last_local.take().unwrap();

                // TODO -> type check target_type and target
                // TODO -> type check target_type and value

                self.symbol_table.reassign_id(target_sym_id, value_sym_id);
            },
            ReturnNode(return_node) => {
                self.visit_return(&return_node);
            },
            ExpressionNode(expr_node) => {
                self.visit_expression(expr_node);
            },
        }
    }

    fn visit_variable_assignment(&mut self, var_assignment: &VariableAssignment) {
        panic!("unimplemented visit_variable_assignment");
    }

    fn visit_return(&mut self, return_stmt: &Return) {
        self.visit_expression(&return_stmt.value);
        self.exiting = true;
    }

    fn visit_expression(&mut self, expr: &Expression)  {
        use crate::syntax::syntax::Expression::*;

        match expr {
            ChainableExpressionNode(cen) => self.visit_chainable_expression(cen),
            UnaryOperationNode(uo) => self.visit_unary_operation(uo),
            LambdaNode(lambda) => self.visit_lambda(lambda),
        }
    }

    fn visit_chainable_expression(&mut self, chainable_expr: &ChainableExpression)  {
        use ExpressionStart::*;
        use ExpressionChain::*;
       
        match chainable_expr.start {
            ConditionalNode(ref cond_node) => self.visit_conditional(cond_node),
            MatchNode(ref match_node) => self.visit_match(match_node),
            LoopNode(ref loop_node) => self.visit_loop(loop_node),
            VariableNode(ref variable) => {
                let name = match variable {
                    Variable::Name(name) => name.clone(),
                    Variable::SelfVariable => "self".to_string(),
                    Variable::SelfType => "Self".to_string(),
                };

                self.last_local = match self.current_env.read().unwrap().get_symbol_by_name(&name) {
                    Some(sym) => Some(sym.clone()),
                    None => panic!(format!("unknown symbol {:?}", name)),
                }
            },
            ValueNode(ref value) => {
                // TODO -> need to revisit temporaries. Shouldn't store them in global symbol
                // table...
                let symbol_id = self.symbol_table.new_symbol(Symbol::Value(value.clone()));
                self.last_local = Some(symbol_id);
            }
        };
        
        for expr_chain in chainable_expr.chained.iter() {
            match expr_chain {
                FieldAccessNode(field_access) => self.visit_field_access(field_access),
                ModuleAccessNode(mod_access) => self.visit_module_access(mod_access),
                ObjectInitializationNode(obj_init) => self.visit_object_initialization(obj_init),
                FunctionApplicationNode(func_app) => self.visit_function_application(func_app),
                TypeApplicationNode(type_app) => self.visit_type_application(type_app),
                BinaryOperationNode(bin_op) => self.visit_binary_operation(bin_op),
            };
        }
    }

    fn visit_conditional(&mut self, _conditional: &Conditional)  {
        panic!("unimplemented visit_conditional");
    }

    fn visit_match(&mut self, _match_node: &Match)  {
        panic!("unimplemented visit_match");
    }

    fn visit_loop(&mut self, _loop_node: &Loop)  {
        panic!("unimplemented visit_loop");
    }

    fn visit_field_access(&mut self, field_access: &FieldAccess)  {
        let to_access_sym_id = self.get_last_local();
        let to_access = self.symbol_table.load_symbol(to_access_sym_id);

        // TODO -> type check that `to_access` is an `ObjectInstance`
       
        let readable_to_access = to_access.read().unwrap();
        if let Symbol::ObjectInstance(obj) = readable_to_access.deref() {
            if obj.field_values.contains_key(&field_access.field_name) {
                let sym_id = obj.field_values.get(&field_access.field_name).unwrap();
                self.last_local = Some(*sym_id);
            } else {
                let message = format!("unknown field {} of object.", field_access.field_name);
                panic!(message);
            }
        } else {
            let message = format!("unable to read symbol {:?}", to_access);
            panic!(message);
        }
    }

    fn visit_module_access(&mut self, _mod_access: &ModuleAccess)  {
        panic!("unimplemented visit_module_access");
    }

    fn visit_object_initialization(&mut self, obj_init: &ObjectInitialization)  {
        let last_local_sym_id = self.last_local.take().unwrap();
        let last_local = self.symbol_table.load_symbol(last_local_sym_id);

        let readable_last_local = last_local.read().unwrap();

        let obj_decl = if let Symbol::Object(obj_decl) = readable_last_local.deref() {
            obj_decl
        } else {
            panic!("trying to initialize object on symbol that isn't an object: {:?}", self.last_local)
        };

        // ensure that number of fields match
        if obj_decl.fields.len() != obj_init.fields.len() {
            let msg = format!("expected {} fields in object initialization but got {}",obj_decl.fields.len(), obj_init.fields.len());
            panic!(msg);
        }

        for (key, _val) in obj_decl.fields.iter() {
            // ensure names of fields match
            if !obj_init.fields.contains_key(key) {
                let msg = format!("expected field {} in object initialization not present in {:?}", key, obj_init.fields.keys());
                panic!(msg);
            }

            // ensure types of fields match
            // this uses val which is the expected type of the symbol
        }

        let field_values = obj_init.fields
            .iter()
            .map(|(k, v)| {
                self.visit_expression(v);
                let evaluated = self.last_local.take().unwrap();
                (k.clone(), evaluated)
            })
            .collect();

        let initialized_object = ObjectInstance {
            field_values,
        };

        let sym_id = self.symbol_table.new_symbol(Symbol::ObjectInstance(initialized_object));
        self.last_local = Some(sym_id);
    }

    fn visit_function_application(&mut self, func_app: &FunctionApplication)  {
        let last_local_sym_id = self.last_local.take().unwrap();
        let last_local = self.symbol_table.load_symbol(last_local_sym_id);

        // TODO -> Clean this up. duplicated code but dont have a nice abstraction
        let last_local_readable = last_local.read().unwrap();
        match last_local_readable.deref() {
            Symbol::Function(func) => {
                let result_sym_id = func.call(self, &func_app.args);

                if let Some(ref _ret_type) = func.returns {
                    // TODO -> type check return value
                    self.last_local = Some(result_sym_id.unwrap());
                }
            },
            Symbol::NativeFunction(native_func) => {
                let _result_sym_id = native_func.call(self, &func_app.args);
            },
            _ => panic!("attempting to apply arguments to something that isn't a function")
        }
    }

    fn visit_type_application(&mut self, type_app: &TypeApplication) {
        let to_apply_to_sym_id = self.last_local.take().unwrap();
        let to_apply_to = self.symbol_table.load_symbol(to_apply_to_sym_id);

        // can only apply to functions, objects, enums, ???

        // TODO -> check num type params versus num type args
        match to_apply_to.read().unwrap().deref() {
            Symbol::Object(o) => {
                if o.type_arguments.len() != type_app.args.len() {
                    let message = format!("invalid number of type parameters in type application. expected {}, got {}", o.type_arguments.len(), type_app.args.len());
                    panic!(message);
                }
            },
            Symbol::Enum(e) => {
                if e.type_arguments.len() != type_app.args.len() {
                    let message = format!("invalid number of type parameters in type application. expected {}, got {}", e.type_arguments.len(), type_app.args.len());
                    panic!(message);
                }
            },
            Symbol::Function(_f) => {
                panic!("unimplemented");
            },
            _ => panic!("trying to apply types to something that cannot take type arguements"),
        }

        let mut types = Vec::new();
        for arg in type_app.args.iter() {
            self.visit_expression(arg);
            let evaluated_type = self.last_local.take().unwrap();

            // TODO -> type check that results are of type: Type 

            types.push(evaluated_type);
        }

        // NOTE: This is purposefully actually cloning the underlying type as we are creating 
        // a new with one applied types
        let mut new_type = to_apply_to.read().unwrap().deref().clone();

        match new_type {
            Symbol::Object(ref mut o) => {
                o.type_arguments = o.type_arguments
                    .drain(..)
                    .zip(types.into_iter())
                    .map(|((type_name, _), type_to_apply)| (type_name, type_to_apply) )
                    .collect();
            },
            Symbol::Enum(ref mut e) => {
                e.type_arguments = e.type_arguments
                    .drain(..)
                    .zip(types.into_iter())
                    .map(|((type_name, _), type_to_apply)| (type_name, type_to_apply) )
                    .collect();
            },
            Symbol::Function(_f) => {
                panic!("unimplemented");
            },
            _ => panic!("trying to apply types to something that cannot take type arguements"),
        }

        let sym_id = self.symbol_table.new_symbol(new_type);
        self.last_local = Some(sym_id);
    }

    fn visit_unary_operation(&mut self, _unary_op: &UnaryOperation)  {
        panic!("unimplemented visit_unary_operation");
    }

    fn visit_lambda(&mut self, _lambda: &Lambda)  {
        panic!("unimplemented visit_lambda");
    }

    fn visit_binary_operation(&mut self, bin_op: &BinaryOperation)  {
        use BinaryOperator::*;

        let lhs_sym_id = self.last_local.take().unwrap();
        let lhs = self.symbol_table.load_symbol(lhs_sym_id);
        let lhs = match lhs.read().unwrap().deref() {
            Symbol::Value(val) => val.clone(),
            other => panic!(format!("lhs of binary operation needs to be a value but was {:?}", other)),
        };

        self.visit_expression(&bin_op.rhs);
        
        let rhs_sym_id = self.last_local.take().unwrap();
        let rhs = self.symbol_table.load_symbol(rhs_sym_id);
        let rhs = match rhs.read().unwrap().deref() {
            Symbol::Value(val) => val.clone(),
            other => panic!(format!("rhs of binary operation needs to be a value but was {:?}", other)),
        };

        let result = match bin_op.op {
            Plus     => Symbol::Value(Value::add(lhs, rhs)),
            Minus    => Symbol::Value(Value::subtract(lhs, rhs)),
            Multiply => Symbol::Value(Value::multiply(lhs, rhs)),
            Divide   => Symbol::Value(Value::divide(lhs, rhs)),
            Or       => Symbol::Value(Value::or(lhs, rhs)),
            And      => Symbol::Value(Value::and(lhs, rhs)),
        };

        let sym_id = self.symbol_table.new_symbol(result);
        self.last_local = Some(sym_id);
    }
}

impl Callable<Interpreter> for Object {
    fn call(&self, _interpreter: &mut Interpreter, _args: &Vec<Expression>) -> Option<SymbolId> {
        panic!("unimplemented");
    }
}

impl Callable<Interpreter> for Enum {
    fn call(&self, _interpreter: &mut Interpreter, _args: &Vec<Expression>) -> Option<SymbolId> {
        panic!("unimplemented");
    }
}

impl Callable<Interpreter> for Function {
    fn call(&self, interpreter: &mut Interpreter, args: &Vec<Expression>) -> Option<SymbolId> {
        // TODO -> check visibility

        if self.parameters.len() != args.len() {
            let message = format!("invalid number of parameters in function application. expected {}, got {}", self.parameters.len(), args.len());
            panic!(message);

        }

        // TODO -> type check parameters

        let evaluated_args: Vec<SymbolId> = args.iter()
            .map(|e| {
                interpreter.visit_expression(e);
                interpreter.get_last_local()
            })
            .collect();

        let mut func_app_local_env = Environment::from_parent(&self.environment);
        for (param, arg) in self.parameters.iter().zip(evaluated_args.into_iter()) {
            let name = match param {
                FunctionParameter::SelfParam => String::from(SELF_VAR_SYMBOL_NAME),
                FunctionParameter::TypedVariableDeclarationParam(typed_var) => typed_var.name.clone(),
            };
            
            func_app_local_env.define(name, arg);
        }

        interpreter.set_current_env(func_app_local_env);
        interpreter.visit_block_body(&self.body);
        
        if let Some(_type_sym) = &self.returns {
            // TODO -> type check return based on the expected `type_sym`
            Some(interpreter.get_last_local())
        } else {
            None
        }
    }
}

impl Callable<Interpreter> for NativeFunction {
    fn call(&self, interpreter: &mut Interpreter, args: &Vec<Expression>) -> Option<SymbolId> {
        match self {
            NativeFunction::Print => {
                if args.len() != 1 {
                    let message = format!("invalid number of parameters function call. expected 1, got {}", args.len());
                    panic!(message);
                }

                let result_sym_id = {
                    interpreter.visit_expression(args.iter().nth(0).unwrap());
                    interpreter.get_last_local()
                };

                let result = interpreter.symbol_table.load_symbol(result_sym_id);

                let readable_result = result.read().unwrap();
                match readable_result.deref() {
                    Symbol::Value(Value::StringValue(s)) => {
                        println!("{}", s);
                        None
                    },
                    sym => panic!("expected a string value but got: {:?}", sym),
                }
            }
        }
    }
}
