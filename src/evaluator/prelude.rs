use std::collections::HashMap;
use std::ops::Deref;

use super::evaluator::{Evaluator, SymbolTable, SymbolId, Symbol, ValueType, NativeFunction};
use crate::evaluator::interpreter::Interpreter;
use crate::syntax::parse_tree::{Expression, Value};

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

    let print_symbol_id = symbol_table.new_symbol(Symbol::NativeFunction(print_nat_fn()));
    env.insert("print".into(), print_symbol_id);

    env
}

fn print_nat_fn() -> NativeFunction {
    NativeFunction {
        name: "print".into(),
        args: Vec::new(),
        func: |interp: &mut Interpreter, args: Vec<SymbolId>| {
            if args.len() != 1 {
                let message = format!(
                    "invalid number of parameters function call. expected 1, got {}",
                    args.len()
                );
                panic!(message);
            }

            let result_sym_id = args.iter().nth(0).unwrap();
            let result = interp.symbol_table.load_symbol(*result_sym_id);

            let readable_result = result.read().unwrap();
            match readable_result.deref() {
                Symbol::Value(Value::StringValue(s)) => {
                    println!("{}", s);
                    None
                }
                sym => panic!("expected a string value but got: {:?}", sym),
            }
        },
    }
}

