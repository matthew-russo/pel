use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::sync::{Arc, RwLock};

use crate::evaluator::evaluator::{
    Callable,
    Contract,
    Enum,
    EnumInstance,
    Environment,
    Evaluator,
    Function,
    FunctionInvocation,
    FunctionSignature,
    NativeFunction,
    Object,
    ObjectInstance,
    Symbol,
    SymbolId,
    SymbolTable,
    SymHash,
    VTables,
    VTable,
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
    pub vtables: VTables,
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
            vtables: VTables::new(),
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

            ContractDeclarationNode(contract_decl) => self.visit_contract_declaration(contract_decl),

            ImplementationDeclarationNode(impl_decl) => self.visit_implementation_declaration(impl_decl),

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

        let mut enu = Enum {
            parent: SymbolTable::MAIN_MODULE_SYMBOL_ID,
            name: enum_decl.type_name.clone(),
            type_arguments,
            variants,
            variant_funcs: HashMap::new(),
            methods: HashMap::new(),
            vtables: HashMap::new(),
        };

        let enu_sym_id = self.symbol_table.gen_id();

        enu.variant_funcs = enum_decl.variants
            .iter()
            .map(|vd| {
                if vd.contains.is_none() {
                    let initialized_enum = EnumInstance {
                        ty: enu_sym_id,
                        contract_ty: None,
                        variant: (vd.name.clone(), None),
                    };

                    (vd.name.clone(), Symbol::EnumInstance(initialized_enum))
                } else {
                    let enum_constructor = |interp: &mut Interpreter, args: Vec<SymbolId>| {
                        let ty_sym_id = args.iter().nth(0).unwrap();
                        let ty_sym = interp.symbol_table.load_symbol(*ty_sym_id);
                        let ty_sym_readable = ty_sym.read().unwrap();
                        let ty = match ty_sym_readable.deref() {
                            Symbol::Value(Value::IntegerValue(i)) => *i as u32,
                            _ => {
                                let message = format!("COMPILER BUG: first argument to enum variant constructor should be an integer -- the SymbolId of the enum -- but was {}",
                                                      ty_sym_readable.sym_hash(&interp.symbol_table).unwrap());
                                panic!(message);
                            },
                        };

                        let name_sym_id = args.iter().nth(1).unwrap();
                        let name_sym = interp.symbol_table.load_symbol(*name_sym_id);
                        let name_sym_readable = name_sym.read().unwrap();
                        let name = match name_sym_readable.deref() {
                            Symbol::Value(Value::StringValue(s)) => s.clone(),
                            _ => {
                                let message = format!("COMPILER BUG: second argument to enum variant constructor should be a string -- the name of the variant -- but was {}",
                                                      name_sym_readable.sym_hash(&interp.symbol_table).unwrap());
                                panic!(message);
                            },
                        };

                        let does_contain_type_sym_id = args.iter().nth(2).unwrap();
                        let does_contain_type_sym = interp.symbol_table.load_symbol(*does_contain_type_sym_id);
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
                            let contains_type_id_sym_id = args.iter().nth(3).unwrap();
                            let contains_type_id_sym = interp.symbol_table.load_symbol(*contains_type_id_sym_id);
                            let contains_type_id_sym_readable = contains_type_id_sym.read().unwrap();
                            let contains_type_id = match contains_type_id_sym_readable.deref() {
                                Symbol::Value(Value::IntegerValue(i)) => *i as u32 as SymbolId,
                                _ => {
                                    let message = format!("COMPILER BUG: fourth argument to enum variant constructor should be an Integer -- the SymbolId of the type it contains -- but was {}",
                                                          contains_type_id_sym_readable.sym_hash(&interp.symbol_table).unwrap());
                                    panic!(message);
                                },
                            };

                            let contains_sym_id = args.iter().nth(4).unwrap();
                            let contains_sym = interp.symbol_table.load_symbol(*contains_sym_id);
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
                            
                            Some(*contains_sym_id)
                        } else {
                            None
                        };
                        
                        let initialized_enum = EnumInstance {
                            ty,
                            contract_ty: None,
                            variant: (name, contains),
                        };

                        Some(interp.symbol_table.new_symbol(Symbol::EnumInstance(initialized_enum)))
                    };

                    let nat_func = NativeFunction {
                        name: "".into(),
                        args: Vec::new(),
                        func: enum_constructor,
                    };

                    (vd.name.clone(), Symbol::NativeFunction(nat_func))
                }
            })
            .collect();

        enu.methods = enum_decl
            .methods
            .iter()
            .map(|fd| {
                self.last_local = Some(enu_sym_id);
                self.visit_function_declaration(fd);
                let method_id = self.last_local.take().unwrap();
                (fd.signature.name.clone(), method_id)
            })
            .collect();

        let enu_sym = Symbol::Enum(enu);

        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;

        self.symbol_table.new_symbol_with_id(enu_sym, enu_sym_id);
        self.current_env
            .write()
            .unwrap()
            .define(enum_decl.type_name.clone(), enu_sym_id);
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

        let obj = Object {
            parent: SymbolTable::MAIN_MODULE_SYMBOL_ID,
            name: obj_decl.type_name.clone(),
            type_arguments,
            fields,
            methods: HashMap::new(),
            vtables: HashMap::new(),
        };

        let obj_sym_id = self.symbol_table.new_symbol(Symbol::Object(obj));

        let methods = obj_decl.methods
            .iter()
            .map(|fd| {
                self.last_local = Some(obj_sym_id);
                self.visit_function_declaration(fd);
                let method_id = self.last_local.take().unwrap();
                (fd.signature.name.clone(), method_id)
            })
            .collect();

        let obj_sym = self.symbol_table.load_symbol(obj_sym_id);
        let mut obj_sym_writable = obj_sym.write().unwrap();
        if let Symbol::Object(ref mut o) = obj_sym_writable.deref_mut() {
            o.methods = methods;
        }

        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;

        self.current_env
            .write()
            .unwrap()
            .define(obj_decl.type_name.clone(), obj_sym_id);
    }

    fn visit_contract_declaration(&mut self, contract_decl: &ContractDeclaration) {
        let mut local_env = Environment::from_parent(&self.current_env);

        let type_arguments = contract_decl.type_params
            .iter()
            .map(|type_param_name| {
                let sym_hole_id = self.symbol_table.new_symbol(Symbol::TypeVariable(None));
                local_env.define(type_param_name.clone(), sym_hole_id);
                (type_param_name.clone(), sym_hole_id)
            })
            .collect();

        self.current_env = Arc::new(RwLock::new(local_env));

        let contract = Contract {
            parent: SymbolTable::MAIN_MODULE_SYMBOL_ID,
            name: contract_decl.type_name.clone(),
            type_arguments,
            required_functions: Vec::new(),
        };

        let contract_sym_id = self.symbol_table.new_symbol(Symbol::Contract(contract));

        let required_functions = contract_decl.functions
                .iter()
                .map(|fs| {
                    self.last_local = Some(contract_sym_id);
                    self.visit_function_signature(&fs);
                    self.last_local.take().unwrap()
                })
                .collect();

        let contract_sym = self.symbol_table.load_symbol(contract_sym_id);
        let mut contract_sym_writable = contract_sym.write().unwrap();
        if let Symbol::Contract(ref mut c) = contract_sym_writable.deref_mut() {
            c.required_functions = required_functions;
        }

        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;

        self.last_local = Some(contract_sym_id);

        self.current_env
            .write()
            .unwrap()
            .define(contract_decl.type_name.clone(), contract_sym_id);
    }

    fn visit_implementation_declaration(&mut self, impl_decl: &ImplementationDeclaration) {
        let local_env = Environment::from_parent(&self.current_env);
        self.current_env = Arc::new(RwLock::new(local_env));

        self.visit_expression(&impl_decl.implementing_type);
        let implementor_type_sym_id = self.last_local.take().unwrap();
        let implementor_type_sym = self.symbol_table.load_symbol(implementor_type_sym_id);

        self.visit_expression(&impl_decl.contract);
        let contract_sym_id = self.last_local.take().unwrap();
        let contract_sym = self.symbol_table.load_symbol(contract_sym_id);

        let vtable = VTable {
            implementor: implementor_type_sym_id,
            implementing: contract_sym_id,
            functions: HashMap::new(),
        };

        let vptr = self.vtables.new_vtable(vtable);

        {
            let mut implementor_type_writable = implementor_type_sym.write().unwrap();
            match implementor_type_writable.deref_mut() {
                Symbol::Object(o) => {
                    o.vtables.insert(contract_sym_id, vptr);
                },
                Symbol::Enum(e) => {
                    e.vtables.insert(contract_sym_id, vptr);
                },
                s => {
                    let message = format!("Expected implementing type to be an Object or Enum but got: {}", s.sym_hash(&self.symbol_table).unwrap());
                    panic!(message);
                },
            }
        }

        let required_functions_by_name: HashMap<String, SymbolId> = if let Symbol::Contract(c) = contract_sym.read().unwrap().deref() {
            c.required_functions
                .iter()
                .map(|sym_id| {
                    let req_func_sym = self.symbol_table.load_symbol(*sym_id);
                    let req_func_sym_readable = req_func_sym.read().unwrap();
                    return if let Symbol::FunctionSignature(fs) = req_func_sym_readable.deref() {
                        (fs.name.clone(), *sym_id)
                    } else {
                        let message = format!("COMPILER BUG: Contract required function is not a FunctionSignature: {:?}", req_func_sym);
                        panic!(message);
                    }
                })
                .collect()
        } else {
            let message = format!("Expected Contract implementation to be a Contract but got {}", contract_sym.read().unwrap().get_type());
            panic!(message);
        };

        let implementing_functions_by_name: HashMap<String, SymbolId> = impl_decl.functions
            .iter()
            .map(|fd| {
                self.last_local = Some(implementor_type_sym_id);
                self.visit_function_declaration(fd);
                let sym_id = self.last_local.take().unwrap();
                let impl_func_sym = self.symbol_table.load_symbol(sym_id);
                let impl_func_sym_readable = impl_func_sym.read().unwrap();
                return if let Symbol::Function(f) = impl_func_sym_readable.deref() {
                    let impl_sig_sym = self.symbol_table.load_symbol(f.signature);
                    let impl_sig_sym_readable = impl_sig_sym.read().unwrap();
                    let impl_sig = if let Symbol::FunctionSignature(fs) = impl_sig_sym_readable.deref() {
                        fs
                    } else {
                        let message = format!("COMPILER BUG: Siganture of function: {:?} is not a FunctionSignature: {:?}", f, impl_sig_sym);
                        panic!(message);
                    };

                    (impl_sig.name.clone(), sym_id)
                } else {
                    let message = format!("COMPILER BUG: Contract required function is not a FunctionSignature: {:?}", impl_func_sym);
                    panic!(message);
                } 
            })
            .collect();

        for (req_name, req_sig_id) in required_functions_by_name.iter() {
            let req_sig_sym = self.symbol_table.load_symbol(*req_sig_id);
            let req_sig_sym_readable = req_sig_sym.read().unwrap();
            let req_sig = if let Symbol::FunctionSignature(f) = req_sig_sym_readable.deref() {
                f
            } else {
                let message = format!("COMPILER BUG: Symbol pointing to required function signature is a {} but expected a FunctionSignature", 
                                      req_sig_sym.read().unwrap().sym_hash(&self.symbol_table).unwrap());
                panic!(message);
            };

            if !implementing_functions_by_name.contains_key(req_name) {
                let message = format!("Contract implementation for {} by {} requires function {}",
                                      contract_sym.read().unwrap().sym_hash(&self.symbol_table).unwrap(),
                                      implementor_type_sym.read().unwrap().sym_hash(&self.symbol_table).unwrap(),
                                      req_sig.sym_hash(&self.symbol_table).unwrap());
                panic!(message);
            }

            let implementor_id = implementing_functions_by_name.get(req_name).unwrap();
            let implementor_func_sym = self.symbol_table.load_symbol(*implementor_id);
            let implementor_func_sym_readable = implementor_func_sym.read().unwrap();
            let implementor_func = if let Symbol::Function(f) = implementor_func_sym_readable.deref() {
                f
            } else {
                let message = format!("COMPILER BUG: Symbol pointed to implementing function is a {} but expected a Function", 
                                      implementor_func_sym_readable.sym_hash(&self.symbol_table).unwrap());
                panic!(message);
            };

            let implementor_func_sig_sym = self.symbol_table.load_symbol(implementor_func.signature);
            let implementor_func_sig_sym_readable = implementor_func_sig_sym.read().unwrap();
            let implementor_func_sig = if let Symbol::FunctionSignature(fs) = implementor_func_sig_sym_readable.deref() {
                fs
            } else {
                
                let message = format!("COMPILER BUG: Symbol pointing function signature is a {} but expected a FunctionSignature", 
                                      implementor_func_sig_sym_readable.sym_hash(&self.symbol_table).unwrap());
                panic!(message);
            };
         
            if !req_sig.is_compatible_with(&implementor_func_sig, &self.symbol_table) {
                let message = format!("Function with name: {} has signature: {} but expected: {}",
                                      req_name,
                                      req_sig.sym_hash(&self.symbol_table).unwrap(),
                                      implementor_func_sig.sym_hash(&self.symbol_table).unwrap());
                panic!(message);
            }
        }

        for (_impl_name, func_id) in implementing_functions_by_name.iter() {
            let func_sym = self.symbol_table.load_symbol(*func_id);
            let func_sym_readable = func_sym.read().unwrap();
            let func = if let Symbol::Function(f) = func_sym_readable.deref() {
                f
            } else {
                let message = format!("COMPILER BUG: Symbol pointed to implementing function is a {} but expected a Function", 
                                      func_sym_readable.sym_hash(&self.symbol_table).unwrap());
                panic!(message);
            };

            let func_sig_sym = self.symbol_table.load_symbol(func.signature);
            let func_sig_sym_readable = func_sig_sym.read().unwrap();
            let func_sig = if let Symbol::FunctionSignature(fs) = func_sig_sym_readable.deref() {
                fs
            } else {
                let message = format!("COMPILER BUG: Symbol pointed to by signature of function: {} is a {} but expected a FunctionSignature", 
                                      func.sym_hash(&self.symbol_table).unwrap(),
                                      func_sig_sym_readable.sym_hash(&self.symbol_table).unwrap());
                panic!(message);
            };

            if !required_functions_by_name.contains_key(&func_sig.name) {
                let message = format!("Contract implementation for {} by {} contains unknown function: {}",
                                      contract_sym.read().unwrap().sym_hash(&self.symbol_table).unwrap(),
                                      implementor_type_sym.read().unwrap().sym_hash(&self.symbol_table).unwrap(),
                                      func.sym_hash(&self.symbol_table).unwrap());
                panic!(message);
            }
        }
    
        let vtable = self.vtables.load_vtable(vptr);
        vtable.write().unwrap().functions = implementing_functions_by_name;

        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;
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
            let message = format!("function '{}' already defined in the current environment",
                                  name);
            panic!(message);
        }

        self.visit_function_signature(&func_decl.signature);
        let func_sig_id = self.last_local.take().unwrap();

        let func = Function {
            // TODO -> remove this and pass module context in
            parent: SymbolTable::MAIN_MODULE_SYMBOL_ID,
            signature: func_sig_id,
            body: func_decl.body.clone(),
            environment: self.current_env.clone(),
        };

        let func_sym = Symbol::Function(func);

        let symbol_id = self.symbol_table.new_symbol(func_sym);
        self.current_env
            .write()
            .unwrap()
            .define(name.clone(), symbol_id);
        self.last_local = Some(symbol_id);
    }

    fn visit_function_signature(&mut self, func_sig: &crate::syntax::parse_tree::FunctionSignature) {
        let type_params = func_sig.type_parameters
            .iter()
            .map(|t| (t.clone(), SymbolTable::EMPTY_TYPE_VARIABLE_SYMBOL_ID))
            .collect();

        let mut parameters = Vec::new();
        for param in func_sig.parameters.iter() {
            let p = match param {
                FunctionParameter::SelfParam => {
                    let current_sym_id = self.last_local.take().unwrap();
                    (SELF_VAR_SYMBOL_NAME.into(), current_sym_id)
                },
                FunctionParameter::TypedVariableDeclarationParam(tvd) => {
                    self.visit_expression(&tvd.type_reference);
                    (tvd.name.clone(), self.last_local.take().unwrap())
                }
            };

            parameters.push(p);
        }

        let returns = match func_sig.returns {
            Some(ref expr) => {
                self.visit_expression(&expr);
                Some(self.last_local.take().unwrap())
            },
            None => None,
        };

        let func_sig = FunctionSignature {
            name: func_sig.name.clone(),
            type_parameters: type_params,
            parameters,
            returns,
        };

        let func_sig_id = self.symbol_table.get_or_create_symbol(Symbol::FunctionSignature(func_sig));
        self.last_local = Some(func_sig_id);
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
            VariableAssignmentNode(var_assignment) =>
                self.visit_variable_assignment(var_assignment),

            ReturnNode(return_node) =>
                self.visit_return(&return_node),

            ExpressionNode(expr_node) =>
                self.visit_expression(expr_node),
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
            let value_type_sym = self.symbol_table.load_symbol(value_type_sym_id);
            let value_type_sym_writable = value_type_sym.write().unwrap();
            let is_error_state = match value_type_sym_writable.deref() {
                Symbol::Object(o) => !o.vtables.contains_key(&target_type_sym_id),
                Symbol::Enum(e) => !e.vtables.contains_key(&target_type_sym_id),
                _ => true,
            };

            if is_error_state {
                let target_type_sym = self.symbol_table.load_symbol(target_type_sym_id);
                panic!("expected a value of type: {}, but got {}",
                       target_type_sym.read().unwrap().sym_hash(&self.symbol_table).unwrap(),
                       value_type_sym.read().unwrap().sym_hash(&self.symbol_table).unwrap());
            } else {
                let mut value_writable = value.write().unwrap();
                match value_writable.deref_mut() {
                    Symbol::ObjectInstance(ref mut oi) => oi.contract_ty = Some(target_type_sym_id),
                    Symbol::EnumInstance(ref mut ei) => ei.contract_ty = Some(target_type_sym_id),
                    _ => (),
                }
            }
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
                    Some(sym) => {
                        Some(sym.clone())
                    },
                    None => {
                        let message = format!("unknown symbol {:?}", name);
                        panic!(message);
                    },
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

    fn visit_match(&mut self, match_node: &Match) {
        panic!("unimplemented visit_match");
        self.visit_expression(&match_node.expr);
        let target_sym_id = self.last_local.take().unwrap();
        let target_sym = self.symbol_table.load_symbol(target_sym_id);
        let target_sym_readable = target_sym.read().unwrap();

        for pattern in match_node.patterns.iter() {
            self.visit_expression(&pattern.to_match);
            let to_match_sym_id = self.last_local.take().unwrap();
            let to_match_sym = self.symbol_table.load_symbol(to_match_sym_id);

            
        }
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
        match readable_to_access.deref() {
            Symbol::SelfVariable(sym_id) => {
                // self variables can access fields and call methods
                let oi_sym = self.symbol_table.load_symbol(*sym_id);
                let oi_sym_readable = oi_sym.read().unwrap();
                if let Symbol::ObjectInstance(oi) = oi_sym_readable.deref() {
                    match oi.field_values.get(&field_access.field_name) {
                        Some(v) => {
                            self.last_local = Some(*v);
                            return;
                        },
                        None => (),
                    }

                    let obj = self.symbol_table.load_symbol(oi.ty);
                    let obj_readable = obj.read().unwrap();
                    if let Symbol::Object(o) = obj_readable.deref() {
                        match o.methods.get(&field_access.field_name) {
                            Some(func_id) => {
                                let self_sym = Symbol::SelfVariable(to_access_sym_id);
                                let self_sym_id = self.symbol_table.new_symbol(self_sym);
                                let func_invoc = FunctionInvocation {
                                    func_id: *func_id,
                                    args: vec![self_sym_id],
                                };
                                let sym_id = self.symbol_table.new_symbol(Symbol::FunctionInvocation(func_invoc));
                                self.last_local = Some(sym_id);
                                return;
                            },
                            None => (),
                        }
                    }

                    panic!("self variable of type: {} does not have field or method with name: {}",
                           obj_readable.sym_hash(&self.symbol_table).unwrap(),
                           field_access.field_name);
                } else {
                    // TODO -> we also need Enums
                    panic!("expected self variable to point to an ObjectInstance but got {}",
                           oi_sym_readable.sym_hash(&self.symbol_table).unwrap());
                }
            },
            Symbol::ObjectInstance(oi) => {
                // only methods can be called on object instances that aren't self
                let obj_sym = self.symbol_table.load_symbol(oi.ty);
                let obj_sym_readable = obj_sym.read().unwrap();
                let obj = match obj_sym_readable.deref() {
                    Symbol::Object(o) => o,
                    s => panic!("COMPILER BUG: parent of ObjectInstance must be an object but got {}", s.sym_hash(&self.symbol_table).unwrap()),
                };

                if let Some(c_id) = oi.contract_ty {
                    let vtable_id = match obj.vtables.get(&c_id) {
                        Some(vt) => vt,
                        None => {
                            let message = format!("COMPILER BUG: contract_ty field of Object {} is populated but does not contain vtable",
                                                  obj.sym_hash(&self.symbol_table).unwrap());
                            panic!(message);
                        },
                    };

                    let vtable = self.vtables.load_vtable(*vtable_id);
                    let vtable_readable = vtable.read().unwrap();
                    match vtable_readable.functions.get(&field_access.field_name) {
                        Some(func_id) => {
                            let self_sym = Symbol::SelfVariable(to_access_sym_id);
                            let self_sym_id = self.symbol_table.new_symbol(self_sym);
                            let func_invoc = FunctionInvocation {
                                func_id: *func_id,
                                args: vec![self_sym_id],
                            };
                            let sym_id = self.symbol_table.new_symbol(Symbol::FunctionInvocation(func_invoc));
                            self.last_local = Some(sym_id);
                            return;
                        },
                        None => {
                            let contract_sym = self.symbol_table.load_symbol(c_id);
                            let message = format!("unknown method '{}' of object: {}.",
                                                  field_access.field_name,
                                                  contract_sym.read().unwrap().sym_hash(&self.symbol_table).unwrap());
                            panic!(message);
                        },
                    }
                } else {
                    match obj.methods.get(&field_access.field_name) {
                        Some(func_id) => {
                            let self_sym = Symbol::SelfVariable(to_access_sym_id);
                            let self_sym_id = self.symbol_table.new_symbol(self_sym);
                            let func_invoc = FunctionInvocation {
                                func_id: *func_id,
                                args: vec![self_sym_id],
                            };
                            let sym_id = self.symbol_table.new_symbol(Symbol::FunctionInvocation(func_invoc));
                            self.last_local = Some(sym_id);
                            return;
                        },
                        None => (),
                    }

                    for (_, vtable_id) in obj.vtables.iter() {
                        let vtable = self.vtables.load_vtable(*vtable_id);
                        let vtable_readable = vtable.read().unwrap();
                        match vtable_readable.functions.get(&field_access.field_name) {
                            Some(func_id) => {
                                let self_sym = Symbol::SelfVariable(to_access_sym_id);
                                let self_sym_id = self.symbol_table.new_symbol(self_sym);
                                let func_invoc = FunctionInvocation {
                                    func_id: *func_id,
                                    args: vec![self_sym_id],
                                };
                                let sym_id = self.symbol_table.new_symbol(Symbol::FunctionInvocation(func_invoc));
                                self.last_local = Some(sym_id);
                                return;
                            },
                            None => (),
                        }
                    }

                    let message = format!("unknown method '{}' of object: {}.",
                                      field_access.field_name,
                                      obj.sym_hash(&self.symbol_table).unwrap());
                    panic!(message);
                }
            },
            // Symbol::DataInstance {
            //     field access
            // },
            _ => {
                let message = format!("trying to use object initialization syntax on something that isn't an object: {:?}", to_access);
                panic!(message);
            }
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
                    Some(Symbol::EnumInstance(ei)) => {
                        let id = self.symbol_table.new_symbol(Symbol::EnumInstance(EnumInstance::clone(ei)));
                        self.last_local = Some(id);
                    },
                    Some(Symbol::NativeFunction(func)) => {
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
                    Some(sym) => {
                        let message = format!("COMILER BUG: Enum {} has variant {} but it is {} and not an EnumInstance or NativeFunction",
                                              enu.sym_hash(&self.symbol_table).unwrap(),
                                              mod_access.name,
                                              sym.sym_hash(&self.symbol_table).unwrap());
                        panic!(message);
                    }
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
            contract_ty: None,
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
                let args = func_app.args
                    .iter()
                    .map(|expr| {
                        self.visit_expression(expr);
                        self.last_local.take().unwrap()
                    })
                    .collect();

                let result_sym_id = func.call(self, args);

                let signature_sym = self.symbol_table.load_symbol(func.signature);
                let signature_sym_readable = signature_sym.read().unwrap();
                let signature = if let Symbol::FunctionSignature(sig) = signature_sym_readable.deref() {
                    sig
                } else {
                    let message = format!("COMPILER ERROR: expected FunctionSignature but got {}", signature_sym_readable.sym_hash(&self.symbol_table).unwrap());
                    panic!(message);
                };

                if let Some(ref _ret_type) = signature.returns {
                    // TODO -> type check return value
                    self.last_local = Some(result_sym_id.unwrap());
                }
            },
            Symbol::NativeFunction(native_func) => {
                let all_args = vec![
                        native_func.args.clone(),
                        func_app.args.clone()]
                    .concat()
                    .iter()
                    .map(|expr| {
                        self.visit_expression(expr);
                        self.last_local.take().unwrap()
                    })
                    .collect();

                let result_sym_id = native_func.call(self, all_args);

                if let Some(sym_id) = result_sym_id {
                    // TODO -> type check return value
                    self.last_local = Some(sym_id);
                }
            },
            Symbol::FunctionInvocation(fi) => {
                let new_args = func_app.args
                    .iter()
                    .map(|expr| {
                        self.visit_expression(expr);
                        self.last_local.take().unwrap()
                    })
                    .collect();

                let actual_args = [
                    fi.args.clone(),
                    new_args,
                ].concat();

                let func_sym = self.symbol_table.load_symbol(fi.func_id);
                let func_sym_readable = func_sym.read().unwrap();
                match func_sym_readable.deref() {
                    Symbol::Function(func) => {
                        let signature_sym = self.symbol_table.load_symbol(func.signature);
                        let signature_sym_readable = signature_sym.read().unwrap();
                        let signature = if let Symbol::FunctionSignature(sig) = signature_sym_readable.deref() {
                            sig
                        } else {
                            let message = format!("COMPILER ERROR: expected FunctionSignature but got {}", signature_sym_readable.sym_hash(&self.symbol_table).unwrap());
                            panic!(message);
                        };

                        let result_sym_id = func.call(self, actual_args);
                        if let Some(ref _ret_type) = signature.returns {
                            // TODO -> type check return value
                            self.last_local = Some(result_sym_id.unwrap());
                        }
                    },
                    s => {
                        let message = format!("COMPILER BUG: function invocation not pointing to a function, pointing to: {}",
                                              s.sym_hash(&self.symbol_table).unwrap());
                        panic!(message);
                    },
                }
            }
            sym => {
                panic!("attempting to apply arguments to {} which isn't a function",
                          sym.sym_hash(&self.symbol_table).unwrap());
            },
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
            Symbol::Object(obj)     => Symbol::Object(Object::clone(obj)),
            Symbol::Enum(enu)       => Symbol::Enum(Enum::clone(enu)),
            Symbol::Function(_func) => panic!("unimplemented"),
            _ => panic!("trying to apply types to something that cannot take type arguements"),
        };

        // adjust fields and types 
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
            },
            Symbol::Function(_f) => {
                panic!("unimplemented");
            },
            _ => panic!("trying to apply types to something that cannot take type arguements"),
        }

        // we need to check if the equivalent type already exists
        let sym_id = self.symbol_table.get_or_create_symbol(Symbol::clone(&new_type));
        let sym = self.symbol_table.load_symbol(sym_id);
        {
            let mut sym_readable = sym.write().unwrap();
            match sym_readable.deref_mut() {
                Symbol::Enum(ref mut e) => {
                    for (_, variant_sym) in e.variant_funcs.iter_mut() {
                        if let Symbol::EnumInstance(ei) = variant_sym {
                            ei.ty = sym_id.clone();
                        }
                    }
                },
                _ => ()
            }
        }
        self.symbol_table.replace_symbol(sym_id, sym);
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
    fn call(&self, _interpreter: &mut Interpreter, _args: Vec<SymbolId>) -> Option<SymbolId> {
        panic!("unimplemented");
    }
}

impl Callable<Interpreter> for Enum {
    fn call(&self, _interpreter: &mut Interpreter, _args: Vec<SymbolId>) -> Option<SymbolId> {
        panic!("unimplemented");
    }
}

impl Callable<Interpreter> for Function {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<SymbolId>) -> Option<SymbolId> {
        let signature_sym = interpreter.symbol_table.load_symbol(self.signature);
        let signature_sym_readable = signature_sym.read().unwrap();
        let signature = if let Symbol::FunctionSignature(sig) = signature_sym_readable.deref() {
            sig
        } else {
            let message = format!("COMPILER ERROR: expected FunctionSignature but got {}", signature_sym_readable.sym_hash(&interpreter.symbol_table).unwrap());
            panic!(message);
        };
        
        if signature.parameters.len() != args.len() {
            let message = format!(
                "invalid number of parameters in application of function: {}. expected {}, got {}",
                self.sym_hash(&interpreter.symbol_table).unwrap(),
                signature.parameters.len(),
                args.len()
            );
            panic!(message);
        }

        let mut func_app_local_env = Environment::from_parent(&self.environment);
        for ((n, param_id), arg_id) in signature.parameters.iter().zip(args.into_iter()) {
            let param_sym = interpreter.symbol_table.load_symbol(*param_id);
            let param_sym_readable = param_sym.read().unwrap();

            // TODO -> type check load_sym(arg_id).get_type against param_sym

            func_app_local_env.define(n.clone(), arg_id);
        }

        let current_env = Arc::clone(&interpreter.current_env);
        interpreter.set_current_env(func_app_local_env);
        interpreter.visit_block_body(&self.body);
        interpreter.current_env = current_env;
        
        if let Some(_type_sym) = &signature.returns {
            // TODO -> type check return based on the expected `type_sym`
            Some(interpreter.last_local.take().unwrap())
        } else {
            None
        }
    }
}

impl Callable<Interpreter> for NativeFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<SymbolId>) -> Option<SymbolId> {
        (self.func)(interpreter, args)
    }
}

