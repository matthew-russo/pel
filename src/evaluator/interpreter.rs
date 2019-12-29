use std::collections::HashMap;
use std::ops::Deref;
use std::sync::{Arc, RwLock};

use crate::evaluator::evaluator::{
    Callable,
    Enum,
    EnumInstance,
    Environment,
    Evaluator,
    Function,
    NativeFunction,
    Object,
    ObjectInstance,
    Symbol,
    SymbolId,
    SymbolTable,
    SymHash
};
use crate::syntax::parse_tree::*;


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
    pub symbol_table: SymbolTable,
    global_env: Arc<RwLock<Environment>>,
    current_env: Arc<RwLock<Environment>>,
    pub last_local: Option<SymbolId>,
    exiting: bool,
    errors: Vec<String>,
}

const SELF_VAR_SYMBOL_NAME: &str = "self";
const SELF_TYPE_SYMBOL_NAME: &str = "Self";

impl Interpreter {
    pub fn new() -> Self {
        let mut symbol_table = SymbolTable::new();

        let main_mod_sym = symbol_table.load_symbol(SymbolTable::MAIN_MODULE_SYMBOL_ID);
        let main_mod_sym_readable = main_mod_sym.read().unwrap();
        let main_mod = match main_mod_sym_readable.deref() {
            Symbol::Module(m) => m,
            _ => unreachable!(),
        };

        let root_env = Arc::clone(&main_mod.env);

        Self {
            symbol_table,
            global_env: Arc::clone(&root_env),
            current_env: root_env,
            last_local: None,
            exiting: false,
            errors: Vec::new(),
        }
    }

    pub fn emit_error(&mut self, error: String) {
        self.errors.push(error);
    }

    fn var_name_to_define(&mut self, assignment_node: &VariableAssignment) -> Option<String> {
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

        if let None = self.current_env.read().unwrap().get_symbol_by_name(&var_name) {
            Some(var_name) 
        } else {
            None
        }
    }

    pub fn set_current_env(&mut self, env: Environment) {
        self.current_env = Arc::new(RwLock::new(env));
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
            EnumDeclarationNode(enum_decl) => self.visit_enum_declaration(enum_decl),

            ContractDeclarationNode(contract_decl) => {
                self.visit_contract_declaration(contract_decl)
            }

            ImplementationDeclarationNode(impl_decl) => {
                self.visit_implementation_declaration(impl_decl)
            }

            ObjectDeclarationNode(obj_decl) => self.visit_object_declaration(obj_decl),

            FunctionDeclarationNode(func_decl) => self.visit_function_declaration(func_decl),
        }
    }

    fn visit_enum_declaration(&mut self, enum_decl: &EnumDeclaration) {
        let mut local_env = Environment::from_parent(&self.current_env);

        // TODO -> this is duplicated below in visit_object_declaration
        let type_arguments = enum_decl.type_params
            .iter()
            .map(|type_param_name| {
                let sym_hole_id = self.symbol_table.new_symbol(Symbol::TypeVariable(None));
                local_env.define(type_param_name.clone(), sym_hole_id);
                (type_param_name.clone(), sym_hole_id)
            })
            .collect();

        self.current_env = Arc::new(RwLock::new(local_env));

        let variants = enum_decl.variants
            .iter()
            .map(|vd| {
                let contains = if let Some(expr) = &vd.contains {
                    // type check that last_local is a type/kind
                    self.visit_expression(&expr);
                    Some(self.last_local.take().unwrap())
                } else {
                    None
                };

                (vd.name.clone(), contains)
            })
            .collect();

        let variant_funcs = enum_decl.variants
            .iter()
            .map(|vd| {
                let enum_constructor = |interp: &mut Interpreter, args: &Vec<Expression>| {
                    interp.visit_expression(args.iter().nth(0).unwrap());
                    let ty_sym_id = interp.last_local.take().unwrap();
                    let ty_sym = interp.symbol_table.load_symbol(ty_sym_id);
                    let ty_sym_readable = ty_sym.read().unwrap();
                    let ty = match ty_sym_readable.deref() {
                        Symbol::Value(Value::IntegerValue(i)) => *i as u32,
                        _ => {
                            let message = format!("COMPILER BUG: first argument to enum variant constructor should be an integer -- the SymbolId of the enum -- but was {}",
                                                  ty_sym_readable.sym_hash(&interp.symbol_table).unwrap());
                            panic!(message);
                        },
                    };

                    interp.visit_expression(args.iter().nth(1).unwrap());
                    let name_sym_id = interp.last_local.take().unwrap();
                    let name_sym = interp.symbol_table.load_symbol(name_sym_id);
                    let name_sym_readable = name_sym.read().unwrap();
                    let name = match name_sym_readable.deref() {
                        Symbol::Value(Value::StringValue(s)) => s.clone(),
                        _ => {
                            let message = format!("COMPILER BUG: second argument to enum variant constructor should be a string -- the name of the variant -- but was {}",
                                                  name_sym_readable.sym_hash(&interp.symbol_table).unwrap());
                            panic!(message);
                        },
                    };

                    interp.visit_expression(args.iter().nth(2).unwrap());
                    let does_contain_type_sym_id = interp.last_local.take().unwrap();
                    let does_contain_type_sym = interp.symbol_table.load_symbol(does_contain_type_sym_id);
                    let does_contain_type_sym_readable = does_contain_type_sym.read().unwrap();
                    let does_contain_type = match does_contain_type_sym_readable.deref() {
                        Symbol::Value(Value::BooleanValue(b)) => *b,
                        _ => {
                            let message = format!("COMPILER BUG: third argument to enum variant constructor should be a boolean -- whether the variant contains a type -- but was {}",
                                                  does_contain_type_sym_readable.sym_hash(&interp.symbol_table).unwrap());
                            panic!(message);
                        },
                    };

                    let contains = if does_contain_type {
                        interp.visit_expression(args.iter().nth(3).unwrap());
                        let contains_type_id_sym_id = interp.last_local.take().unwrap();
                        let contains_type_id_sym = interp.symbol_table.load_symbol(contains_type_id_sym_id);
                        let contains_type_id_sym_readable = contains_type_id_sym.read().unwrap();
                        let contains_type_id = match contains_type_id_sym_readable.deref() {
                            Symbol::Value(Value::IntegerValue(i)) => *i as u32 as SymbolId,
                            _ => {
                                let message = format!("COMPILER BUG: fourth argument to enum variant constructor should be an Integer -- the SymbolId of the type it contains -- but was {}",
                                                      contains_type_id_sym_readable.sym_hash(&interp.symbol_table).unwrap());
                                panic!(message);
                            },
                        };

                        interp.visit_expression(args.iter().nth(4).unwrap());
                        let contains_sym_id = interp.last_local.take().unwrap();
                        let contains_sym = interp.symbol_table.load_symbol(contains_sym_id);
                        let contains_sym_readable = contains_sym.read().unwrap();
                        if contains_type_id != contains_sym_readable.get_type() {
                            let supposed_to_contain = interp.symbol_table.load_symbol(contains_type_id);
                            let supposed_to_contain_readable = supposed_to_contain.read().unwrap();

                            let actually_contains = interp.symbol_table.load_symbol(contains_sym_readable.get_type());
                            let actually_contains_readable = actually_contains.read().unwrap();
                            let message = format!("expected enum variant: {} to contain type {}, but got {}",
                                                  name,
                                                  supposed_to_contain_readable.sym_hash(&interp.symbol_table).unwrap(),
                                                  actually_contains_readable.sym_hash(&interp.symbol_table).unwrap());
                            panic!(message);
                        }
                        
                        Some(contains_sym_id)
                    } else {
                        None
                    };
                    
                    let initialized_enum = EnumInstance {
                        ty,
                        variant: (name, contains),
                    };

                    Some(interp.symbol_table.new_symbol(Symbol::EnumInstance(initialized_enum)))
                };

                let nat_func = NativeFunction {
                    name: "".into(),
                    args: Vec::new(),
                    func: enum_constructor,
                };
                (vd.name.clone(), nat_func)
            })
            .collect();

        let mut enu = Enum {
            parent: SymbolTable::MAIN_MODULE_SYMBOL_ID,
            name: enum_decl.type_name.clone(),
            type_arguments,
            variants,
            variant_funcs,
            methods: Vec::new(),
        };

        let sym_id = self.symbol_table.gen_id();

        enu.methods = enum_decl
            .methods
            .iter()
            .map(|fd| Function::from_function_decl_syntax(sym_id.clone(), fd, &self.global_env))
            .collect();

        let obj_sym = Symbol::Enum(enu);

        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;

        self.symbol_table.new_symbol_with_id(obj_sym, sym_id);
        self.current_env
            .write()
            .unwrap()
            .define(enum_decl.type_name.clone(), sym_id);
    }

    fn visit_object_declaration(&mut self, obj_decl: &ObjectDeclaration) {
        let mut local_env = Environment::from_parent(&self.current_env);
        
        let type_arguments = obj_decl.type_params
            .iter()
            .map(|type_param_name| {
                let sym_hole_id = self.symbol_table.new_symbol(Symbol::TypeVariable(None));
                local_env.define(type_param_name.clone(), sym_hole_id);
                (type_param_name.clone(), sym_hole_id)
            })
            .collect();


        self.current_env = Arc::new(RwLock::new(local_env));

        let fields = obj_decl
            .fields
            .iter()
            .map(|tvd| {
                self.visit_expression(&tvd.type_reference);
                // type check that last_local is a type/kind
                let evaluated = self.last_local.take().unwrap();
                (tvd.name.clone(), evaluated)
            })
            .collect();

        let mut obj = Object {
            parent: SymbolTable::MAIN_MODULE_SYMBOL_ID,
            name: obj_decl.type_name.clone(),
            type_arguments,
            fields,
            methods: Vec::new(),
        };

        let sym_id = self.symbol_table.gen_id();

        obj.methods = obj_decl
            .methods
            .iter()
            .map(|fd| Function::from_function_decl_syntax(sym_id.clone(), fd, &self.global_env))
            .collect();

        let obj_sym = Symbol::Object(obj);

        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;

        self.symbol_table.new_symbol_with_id(obj_sym, sym_id);
        self.current_env
            .write()
            .unwrap()
            .define(obj_decl.type_name.clone(), sym_id);
    }

    fn visit_contract_declaration(&mut self, contract_decl: &ContractDeclaration) {
        panic!("unimplemented visit_contract_declaration");
        // let contract_sym = Contract::from(contract_decl);
        // self.current_env.define(contract_decl.type_name, contract_sym);
    }

    fn visit_implementation_declaration(&mut self, impl_decl: &ImplementationDeclaration) {
        panic!("unimplemented visit_implementation_declaration");
    }

    fn visit_variant_declaration(&mut self, variant_decl: &VariantDeclaration) {
        panic!("unimplemented visit_variant_declaration");
    }

    fn visit_function_declaration(&mut self, func_decl: &FunctionDeclaration) {
        let name = &func_decl.signature.name;

        let already_exists = {
            self.current_env
                .read()
                .unwrap()
                .get_symbol_by_name(name)
                .is_some()
        };

        if already_exists {
            // let message = format!("function {} already defined at {}", name,
            // sym.defined_at());
            self.emit_error("function already defined".into());
            return;
        }

        let func_sym = Symbol::Function(Function::from_function_decl_syntax(
            // TODO -> remove this and pass module context in
            SymbolTable::MAIN_MODULE_SYMBOL_ID,
            func_decl,
            &self.current_env,
        ));

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

        self.exiting = false;
    }

    fn visit_statement(&mut self, statement: &Statement) {
        use Statement::*;

        match statement {
            VariableAssignmentNode(var_assignment) => {
                self.visit_variable_assignment(var_assignment);
            }
            ReturnNode(return_node) => {
                self.visit_return(&return_node);
            }
            ExpressionNode(expr_node) => {
                self.visit_expression(expr_node);
            }
        }
    }

    fn visit_variable_assignment(&mut self, var_assignment: &VariableAssignment) {
        if let Some(var_name) = self.var_name_to_define(var_assignment) {
            let symbol_id = self
                .symbol_table
                .new_symbol(Symbol::Value(Value::IntegerValue(0)));

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

        let value = self.symbol_table.load_symbol(value_sym_id);
        let value_type_sym_id = value.read().unwrap().get_type();

        if value_type_sym_id != target_type_sym_id {
            let target_type_sym = self.symbol_table.load_symbol(target_type_sym_id);
            let value_type_sym = self.symbol_table.load_symbol(value_type_sym_id);

            panic!("expected a value of type: {}, but got {}",
                   target_type_sym.read().unwrap().sym_hash(&self.symbol_table).unwrap(),
                   value_type_sym.read().unwrap().sym_hash(&self.symbol_table).unwrap());
        }

        self.symbol_table.reassign_id(target_sym_id, value_sym_id);
    }

    fn visit_return(&mut self, return_stmt: &Return) {
        self.visit_expression(&return_stmt.value);
        self.exiting = true;
    }

    fn visit_expression(&mut self, expr: &Expression) {
        use crate::syntax::parse_tree::Expression::*;

        match expr {
            ChainableExpressionNode(cen) => self.visit_chainable_expression(cen),
            UnaryOperationNode(uo) => self.visit_unary_operation(uo),
            LambdaNode(lambda) => self.visit_lambda(lambda),
        }
    }

    fn visit_chainable_expression(&mut self, chainable_expr: &ChainableExpression) {
        use ExpressionChain::*;
        use ExpressionStart::*;

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
            }
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

    fn visit_conditional(&mut self, conditional: &Conditional) {
        for if_expr in conditional.if_exprs.iter() {
            self.visit_expression(&if_expr.condition);
            let cond_sym_id = self.last_local.take().unwrap();
            let cond_sym = self.symbol_table.load_symbol(cond_sym_id);
            let cond_sym_readable = cond_sym.read().unwrap();
            match cond_sym_readable.deref() {
                Symbol::Value(Value::BooleanValue(cond)) => {
                    if *cond {
                        self.visit_block_body(&if_expr.body);
                        return;
                    }
                },
                s => {
                    let message = format!("expected a 'bool' type in if condition but got: {}", s.sym_hash(&self.symbol_table).unwrap());
                    panic!(message);
                },
            }
        }

        if let Some(else_body) = &conditional.else_expr {
            self.visit_block_body(else_body);
        }
    }

    fn visit_match(&mut self, _match_node: &Match) {
        panic!("unimplemented visit_match");
    }

    fn visit_loop(&mut self, loop_node: &Loop) {
        let loop_local_env = Environment::from_parent(&self.current_env);
        self.current_env = Arc::new(RwLock::new(loop_local_env));

        self.visit_variable_assignment(&loop_node.init);
       
        let condition_checker = |interp: &mut Interpreter| {
            interp.visit_expression(&loop_node.condition);
            let cond_sym_id = interp.last_local.take().unwrap();
            let cond_sym = interp.symbol_table.load_symbol(cond_sym_id);
            let cond_sym_readable = cond_sym.read().unwrap();
            return match cond_sym_readable.deref() {
                Symbol::Value(Value::BooleanValue(b)) => *b,
                _ => {
                    let cond_sym_ty = interp.symbol_table.load_symbol(cond_sym_readable.get_type());
                    panic!("expected loop condition to be a 'bool' but got {}",
                            cond_sym_ty.read().unwrap().sym_hash(&interp.symbol_table).unwrap());
                },
            }
        };

        while condition_checker(self) {
            self.visit_block_body(&loop_node.body);
            self.visit_statement(&loop_node.step);
        }

        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;
    }

    fn visit_field_access(&mut self, field_access: &FieldAccess) {
        let to_access_sym_id = self.last_local.take().unwrap();
        let to_access = self.symbol_table.load_symbol(to_access_sym_id);

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
            let message = format!("trying to use object initialization syntax on something that isn't an object: {:?}", to_access);
            panic!(message);
        }
    }

    fn visit_module_access(&mut self, mod_access: &ModuleAccess) {
        let to_access_sym_id = self.last_local.take().unwrap();
        let to_access = self.symbol_table.load_symbol(to_access_sym_id);

        let readable_to_access = to_access.read().unwrap();
        match readable_to_access.deref() {
            Symbol::Module(module) => {
                panic!("MOD_ACCESS: {:?}", mod_access.name);
            },
            Symbol::Enum(enu) => {
                // TODO -> this shouldn't be here. module declarations should create a special
                // hidden module and then enum variant construction is just a function call in to
                // this special function defined inside of a the hidden module

                match enu.variant_funcs.get(&mod_access.name) {
                    Some(func) => {
                        let mut cloned_func = func.clone();

                        let ty = Expression::ChainableExpressionNode(
                            Box::new(ChainableExpression {
                                start: ExpressionStart::ValueNode(Value::IntegerValue(to_access_sym_id as i32)),
                                chained: Vec::new(),
                            })
                        );

                        let name = Expression::ChainableExpressionNode(
                            Box::new(ChainableExpression {
                                start: ExpressionStart::ValueNode(Value::StringValue(mod_access.name.clone())),
                                chained: Vec::new(),
                            })
                        );
                        
                        let variant = enu.variants.get(&mod_access.name).unwrap();
                        let does_contain = Expression::ChainableExpressionNode(
                            Box::new(ChainableExpression {
                                start: ExpressionStart::ValueNode(Value::BooleanValue(variant.is_some())),
                                chained: Vec::new(),
                            })
                        );

                        cloned_func.args = vec![
                            ty,
                            name,
                            does_contain
                        ];

                        if let Some(contains_id) = variant {
                            let contains = self.symbol_table.load_symbol(*contains_id);
                            let contains_readable = contains.read().unwrap();
                            let actually_contains_id = match contains_readable.deref() {
                                Symbol::TypeVariable(maybe_id) => {
                                    match maybe_id {
                                        Some(id) => *id,
                                        None => panic!("trying to get type variable out of hole"),
                                    }
                                },
                                _ => *contains_id,
                            };
                            let contain_ty = Expression::ChainableExpressionNode(
                            Box::new(ChainableExpression {
                                start: ExpressionStart::ValueNode(Value::IntegerValue(actually_contains_id as i32)),
                                chained: Vec::new(),
                            })
                        );
                            cloned_func.args.push(contain_ty);
                        }
                    
                        let id = self.symbol_table.new_symbol(Symbol::NativeFunction(cloned_func));
                        self.last_local = Some(id);
                    },
                    None => {
                        let message = format!("Enum {} has no variant {}",
                                              enu.sym_hash(&self.symbol_table).unwrap(),
                                              mod_access.name);
                        panic!(message);
                    },
                }
            },
            _ => {
            let message = format!("trying to use module access syntax on something that isn't a module: {:?}",
                                  readable_to_access.sym_hash(&self.symbol_table).unwrap());
            }
        }
    }

    fn visit_object_initialization(&mut self, obj_init: &ObjectInitialization) {
        let last_local_sym_id = self.last_local.take().unwrap();
        let last_local = self.symbol_table.load_symbol(last_local_sym_id);

        let readable_last_local = last_local.read().unwrap();

        let obj_decl = if let Symbol::Object(obj_decl) = readable_last_local.deref() {
            obj_decl
        } else {
            panic!(
                "trying to initialize object on symbol that isn't an object: {:?}",
                self.last_local
            )
        };

        // TODO -> need to validate that the object init doesn't have any fields that aren't in the
        // object

        let mut field_values = HashMap::new();
        for (field_name, expected_ty_id_input) in obj_decl.fields.iter() {
            // ensure names of fields match
            if !obj_init.fields.contains_key(field_name) {
                let msg = format!(
                    "expected field '{}' in object initialization not present in {:?}",
                    field_name,
                    obj_init.fields.keys()
                );
                panic!(msg);
            }

            let mut expected_ty_id = expected_ty_id_input.clone();
            let expected_ty = self.symbol_table.load_symbol(expected_ty_id);
            if let Symbol::TypeVariable(maybe_tv_sym_id) = expected_ty.read().unwrap().deref() {
                expected_ty_id = maybe_tv_sym_id.unwrap().clone();
            }

            let field_expr = obj_init.fields.get(field_name).unwrap();
            self.visit_expression(&field_expr);
            let evaluated_id = self.last_local.take().unwrap();
            let evaluated = self.symbol_table.load_symbol(evaluated_id);
            if evaluated.read().unwrap().get_type() != expected_ty_id {
                let expected_ty = self.symbol_table.load_symbol(expected_ty_id);
                let evaluated_ty = self.symbol_table.load_symbol(evaluated.read().unwrap().get_type());

                let msg = format!(
                    "expected field '{}' in object initialization to be type '{}' but got '{}'",
                    field_name,
                    expected_ty.read().unwrap().sym_hash(&self.symbol_table).unwrap(),
                    evaluated_ty.read().unwrap().sym_hash(&self.symbol_table).unwrap(),
                );
                panic!(msg);
            }
            field_values.insert(field_name.clone(), evaluated_id);
        }

        let initialized_object = ObjectInstance {
            ty: last_local_sym_id,
            field_values,
        };

        let sym_id = self
            .symbol_table
            .new_symbol(Symbol::ObjectInstance(initialized_object));
        self.last_local = Some(sym_id);
    }

    fn visit_function_application(&mut self, func_app: &FunctionApplication) {
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
            }
            Symbol::NativeFunction(native_func) => {
                let all_args = vec![
                    native_func.args.clone(),
                    func_app.args.clone()
                ].concat();
                let result_sym_id = native_func.call(self, &all_args);

                if let Some(sym_id) = result_sym_id {
                    // TODO -> type check return value
                    self.last_local = Some(sym_id);
                }
            }
            _ => panic!("attempting to apply arguments to something that isn't a function"),
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
            }
            Symbol::Enum(e) => {
                if e.type_arguments.len() != type_app.args.len() {
                    let message = format!("invalid number of type parameters in type application. expected {}, got {}", e.type_arguments.len(), type_app.args.len());
                    panic!(message);
                }
            }
            Symbol::Function(_f) => {
                panic!("unimplemented");
            }
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
        // TODO -> merge this with below and clean up, reduce duplication
        let to_apply_to_readable = to_apply_to.read().unwrap();
        let mut new_type = match to_apply_to_readable.deref() {
            Symbol::Object(obj)    => Symbol::Object(Object::clone(obj)),
            Symbol::Enum(enu)      => Symbol::Enum(Enum::clone(enu)),
            Symbol::Function(_func) => panic!("unimplemented"),
            _ => panic!("trying to apply types to something that cannot take type arguements"),
        };

        match new_type {
            Symbol::Object(ref mut o) => {
                // create new sym_id
                // update type args to use new sym_id
                // update fields to use new sym_id

                let mut old_sym_id_to_new_sym_id = HashMap::new();
                let mut new_type_args = Vec::new();
                let mut new_fields = HashMap::new();
                let zipped_type_args_with_applied_type = o
                    .type_arguments
                    .iter()
                    .zip(types.into_iter());

                for ((type_name, type_hole_sym_id), type_to_apply) in zipped_type_args_with_applied_type {
                    let new_sym = Symbol::TypeVariable(Some(type_to_apply));
                    let new_sym_id = self.symbol_table.get_or_create_symbol(new_sym);
                    old_sym_id_to_new_sym_id.insert(type_hole_sym_id, new_sym_id);
                    new_type_args.push((type_name.clone(), new_sym_id));

                    for (field_name, field_sym_id) in o.fields.iter() {
                        if field_sym_id == type_hole_sym_id {
                            new_fields.insert(field_name.clone(), new_sym_id);
                        }
                    }
                }

                o.type_arguments = new_type_args;
                o.fields = new_fields;

            },
            Symbol::Enum(ref mut e) => {
                // TODO -> this is copied and pasted from above. need to find where to put this logic

                // create new sym_id
                // update type args to use new sym_id
                // update fields to use new sym_id

                let mut old_sym_id_to_new_sym_id = HashMap::new();
                let mut new_type_args = Vec::new();
                let mut new_variants = HashMap::new();
                let zipped_type_args_with_applied_type = e
                    .type_arguments
                    .iter()
                    .zip(types.into_iter());

                for ((type_name, type_hole_sym_id), type_to_apply) in zipped_type_args_with_applied_type {
                    let new_sym = Symbol::TypeVariable(Some(type_to_apply));
                    let new_sym_id = self.symbol_table.get_or_create_symbol(new_sym);
                    old_sym_id_to_new_sym_id.insert(type_hole_sym_id, new_sym_id);
                    new_type_args.push((type_name.clone(), new_sym_id));

                    for (field_name, maybe_field_sym_id) in e.variants.iter() {
                        if let Some(field_sym_id) = maybe_field_sym_id {
                            if field_sym_id == type_hole_sym_id {
                                new_variants.insert(field_name.clone(), Some(new_sym_id));
                            }
                        } else {
                            new_variants.insert(field_name.clone(), None);
                        }
                    }
                }

                e.type_arguments = new_type_args;
                e.variants = new_variants;
            }
            Symbol::Function(_f) => {
                panic!("unimplemented");
            }
            _ => panic!("trying to apply types to something that cannot take type arguements"),
        }

        // we need to check if the equivalent type already exists
        let sym_id = self.symbol_table.get_or_create_symbol(new_type);
        self.last_local = Some(sym_id);
    }

    fn visit_unary_operation(&mut self, _unary_op: &UnaryOperation) {
        panic!("unimplemented visit_unary_operation");
    }

    fn visit_lambda(&mut self, _lambda: &Lambda) {
        panic!("unimplemented visit_lambda");
    }

    fn visit_binary_operation(&mut self, bin_op: &BinaryOperation) {
        use BinaryOperator::*;

        let lhs_sym_id = self.last_local.take().unwrap();
        let lhs = self.symbol_table.load_symbol(lhs_sym_id);
        let lhs = match lhs.read().unwrap().deref() {
            Symbol::Value(val) => val.clone(),
            other => panic!(format!(
                "lhs of binary operation needs to be a value but was {:?}",
                other
            )),
        };

        self.visit_expression(&bin_op.rhs);

        let rhs_sym_id = self.last_local.take().unwrap();
        let rhs = self.symbol_table.load_symbol(rhs_sym_id);
        let rhs = match rhs.read().unwrap().deref() {
            Symbol::Value(val) => val.clone(),
            other => panic!(format!(
                "rhs of binary operation needs to be a value but was {:?}",
                other
            )),
        };

        let result = match bin_op.op {
            Plus =>               Symbol::Value(Value::add(lhs, rhs)),
            Minus =>              Symbol::Value(Value::subtract(lhs, rhs)),
            Multiply =>           Symbol::Value(Value::multiply(lhs, rhs)),
            Divide =>             Symbol::Value(Value::divide(lhs, rhs)),
            LessThan =>           Symbol::Value(Value::less_than(lhs, rhs)),
            LessThanOrEqual =>    Symbol::Value(Value::less_than_or_equal(lhs, rhs)),
            GreaterThan =>        Symbol::Value(Value::greater_than(lhs, rhs)),
            GreaterThanOrEqual => Symbol::Value(Value::greater_than_or_equal(lhs, rhs)),
            Equal =>              Symbol::Value(Value::equal_to(lhs, rhs)),
            NotEqual =>           Symbol::Value(Value::not_equal_to(lhs, rhs)),
            Or =>                 Symbol::Value(Value::or(lhs, rhs)),
            And =>                Symbol::Value(Value::and(lhs, rhs)),
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
            let message = format!(
                "invalid number of parameters in function application. expected {}, got {}",
                self.parameters.len(),
                args.len()
            );
            panic!(message);
        }

        // TODO -> type check parameters

        let evaluated_args: Vec<SymbolId> = args
            .iter()
            .map(|e| {
                interpreter.visit_expression(e);
                interpreter.last_local.take().unwrap()
            })
            .collect();

        let mut func_app_local_env = Environment::from_parent(&self.environment);
        for (param, arg) in self.parameters.iter().zip(evaluated_args.into_iter()) {
            let name = match param {
                FunctionParameter::SelfParam => String::from(SELF_VAR_SYMBOL_NAME),
                FunctionParameter::TypedVariableDeclarationParam(typed_var) => {
                    typed_var.name.clone()
                }
            };

            func_app_local_env.define(name, arg);
        }

        interpreter.set_current_env(func_app_local_env);
        interpreter.visit_block_body(&self.body);

        if let Some(_type_sym) = &self.returns {
            // TODO -> type check return based on the expected `type_sym`
            Some(interpreter.last_local.take().unwrap())
        } else {
            None
        }
    }
}

impl Callable<Interpreter> for NativeFunction {
    fn call(&self, interpreter: &mut Interpreter, args: &Vec<Expression>) -> Option<SymbolId> {
        (self.func)(interpreter, args)
    }
}
