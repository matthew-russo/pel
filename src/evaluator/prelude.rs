use std::collections::HashMap;

use super::evaluator::{SymbolTable, SymbolId, Symbol, ValueType, NativeFunction};

pub(super) fn prelude(symbol_table: &mut SymbolTable) -> HashMap<String, SymbolId> {
    let mut env = HashMap::new();

    symbol_table.new_symbol_with_id(Symbol::ValueType(ValueType::BooleanType), SymbolTable::BOOL_TYPE_SYMBOL_ID);
    env.insert("bool".into(), SymbolTable::BOOL_TYPE_SYMBOL_ID);

    symbol_table.new_symbol_with_id(Symbol::ValueType(ValueType::CharType), SymbolTable::CHAR_TYPE_SYMBOL_ID);
    env.insert("char".into(), SymbolTable::CHAR_TYPE_SYMBOL_ID);

    symbol_table.new_symbol_with_id(Symbol::ValueType(ValueType::StringType), SymbolTable::STRING_TYPE_SYMBOL_ID);
    env.insert("string".into(), SymbolTable::STRING_TYPE_SYMBOL_ID);

    symbol_table.new_symbol_with_id(Symbol::ValueType(ValueType::IntegerType), SymbolTable::INT_TYPE_SYMBOL_ID);
    env.insert("int".into(), SymbolTable::INT_TYPE_SYMBOL_ID);

    symbol_table.new_symbol_with_id(Symbol::ValueType(ValueType::FloatType), SymbolTable::FLOAT_TYPE_SYMBOL_ID);
    env.insert("float".into(), SymbolTable::FLOAT_TYPE_SYMBOL_ID);



    let print_symbol_id = symbol_table.new_symbol(Symbol::NativeFunction(NativeFunction::Print));
    env.insert("print".into(), print_symbol_id);

    env
}

