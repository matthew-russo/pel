use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use lazy_static;

use super::evaluator::{SymbolTable, SymbolId, Symbol, Object, NativeFunction, Module};

lazy_static! {
    static ref prelude_module: Arc<RwLock<Module>> = Arc::new(RwLock::new(Module { parent: None, name: "main".into(), } ));
}

pub(super) fn prelude(symbol_table: &mut SymbolTable) -> HashMap<String, SymbolId> {
    let mut env = HashMap::new();

    let string_object = Symbol::Object(Arc::new(RwLock::new(string_object())));
    let string_symbol_id = symbol_table.new_symbol(string_object);
    env.insert("String".into(), string_symbol_id);

    let integer_object = Symbol::Object(Arc::new(RwLock::new(integer_object())));
    let integer_symbol_id = symbol_table.new_symbol(integer_object);
    env.insert("Int".into(), integer_symbol_id);


    let print_symbol_id = symbol_table.new_symbol(Symbol::NativeFunction(NativeFunction::Print));
    env.insert("print".into(), print_symbol_id);

    env
}

fn string_object() -> Object {
    let type_arguments = Vec::new();

    let mut fields = HashMap::new();
    fields.insert("internal".into(), SymbolTable::STRING_TYPE_SYMBOL_ID);

    let methods = Vec::new();

    Object {
        module: Arc::clone(&prelude_module),
        name: "String".into(),
        type_arguments,
        fields,
        methods,
    }
}

fn integer_object() -> Object {
    let type_arguments = Vec::new();

    let mut fields = HashMap::new();
    fields.insert("internal".into(), SymbolTable::INT_TYPE_SYMBOL_ID);

    let methods = Vec::new();

    Object {
        module: Arc::clone(&prelude_module),
        name: "Int".into(),
        type_arguments,
        fields,
        methods,
    }
}

// pub fn float_object() -> Object {
// 
// }
// 
// pub fn boolean_object() -> Object {
// 
// }
