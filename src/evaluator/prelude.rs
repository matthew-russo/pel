use std::collections::HashMap;
use std::ops::Deref;

use super::evaluator::{Evaluator, NativeFunction};
use crate::evaluator::interpreter::Interpreter;
use crate::syntax::parse_tree::{Expression, Value};

pub(super) fn prelude(kind_table: &mut KindTable) -> HashMap<String, KindId> {
    let mut env = HashMap::new();

    kind_table.new_symbol_with_id(Kind::ValueType(ValueType::BooleanType), KindTable::BOOL_TYPE_SYMBOL_ID);
    env.insert("bool".into(), KindTable::BOOL_TYPE_SYMBOL_ID);

    kind_table.new_symbol_with_id(Kind::ValueType(ValueType::CharType), KindTable::CHAR_TYPE_SYMBOL_ID);
    env.insert("char".into(), KindTable::CHAR_TYPE_SYMBOL_ID);

    kind_table.new_symbol_with_id(Kind::ValueType(ValueType::StringType), KindTable::STRING_TYPE_SYMBOL_ID);
    env.insert("string".into(), KindTable::STRING_TYPE_SYMBOL_ID);

    kind_table.new_symbol_with_id(Kind::ValueType(ValueType::IntegerType), KindTable::INT_TYPE_SYMBOL_ID);
    env.insert("int".into(), KindTable::INT_TYPE_SYMBOL_ID);

    kind_table.new_symbol_with_id(Kind::ValueType(ValueType::FloatType), KindTable::FLOAT_TYPE_SYMBOL_ID);
    env.insert("float".into(), KindTable::FLOAT_TYPE_SYMBOL_ID);

    let print_symbol_id = kind_table.new_symbol(Kind::NativeFunction(print_nat_fn()));
    env.insert("print".into(), print_symbol_id);

    env
}

fn print_nat_fn() -> NativeFunction {
    NativeFunction {
        name: "print".into(),
        args: Vec::new(),
        func: |interp: &mut Interpreter, args: Vec<Value>| {
            if args.len() != 1 {
                let message = format!(
                    "invalid number of parameters function call. expected 1, got {}",
                    args.len()
                );
                panic!(message);
            }

            let value = args[0];
            let result = interp.kind_table.load_symbol(*result_sym_id);

            let readable_result = result.read().unwrap();
            match readable_result.deref() {
                Kind::Value(Value::StringValue(s)) => {
                    println!("{}", s);
                    None
                }
                sym => panic!("expected a string value but got: {:?}", sym),
            }
        },
    }
}

