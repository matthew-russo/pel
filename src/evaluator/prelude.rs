use std::collections::HashMap;

use super::evaluator::{SymbolTable, SymbolId, Symbol, Object, NativeFunction};

pub(super) fn prelude(symbol_table: &mut SymbolTable) -> HashMap<String, SymbolId> {
    let mut env = HashMap::new();

    let string_object = Symbol::Object(string_object());
    let string_symbol_id = symbol_table.new_symbol(string_object);
    env.insert("String".into(), string_symbol_id);

    let print_symbol_id = symbol_table.new_symbol(Symbol::NativeFunction(NativeFunction::Print));
    env.insert("print".into(), print_symbol_id);

    env
}

fn string_object() -> Object {
    let type_arguments = Vec::new();

    let mut fields = HashMap::new();
    fields.insert("internal".into(), SymbolTable::STRING_TYPE_SYMBOL_ID);

    let functions = Vec::new();

    let methods = Vec::new();

    Object {
        type_arguments,
        fields,
        functions,
        methods,
    }
}

// pub fn integer_object() -> Object {
// 
// }
// 
// pub fn float_object() -> Object {
// 
// }
// 
// pub fn boolean_object() -> Object {
// 
// }
