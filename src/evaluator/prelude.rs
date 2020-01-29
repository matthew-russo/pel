use std::collections::HashMap;

use super::evaluator::{Kind, KindHash, KindTable, Item, NativeFunction, ObjectInstance, ScalarType, Value};
use crate::evaluator::interpreter::Interpreter;

pub(super) fn prelude(kind_table: &mut KindTable) -> HashMap<String, KindHash> {
    let mut env = HashMap::new();

    kind_table.create(Kind::ScalarType(ScalarType::BoolType));
    env.insert(String::from("bool"), ScalarType::bool_kind_hash());

    kind_table.create(Kind::ScalarType(ScalarType::CharType));
    env.insert(String::from("char"), ScalarType::char_kind_hash());

    kind_table.create(Kind::ScalarType(ScalarType::IntegerType));
    env.insert(String::from("int"), ScalarType::int_kind_hash());

    kind_table.create(Kind::ScalarType(ScalarType::LongType));
    env.insert(String::from("long"), ScalarType::long_kind_hash());

    kind_table.create(Kind::ScalarType(ScalarType::FloatType));
    env.insert(String::from("float"), ScalarType::float_kind_hash());

    kind_table.create(Kind::ScalarType(ScalarType::DoubleType));
    env.insert(String::from("double"), ScalarType::double_kind_hash());

    let print_symbol_id = kind_table.create(Kind::NativeFunction(print_nat_fn()));
    env.insert("print".into(), print_symbol_id);

    env
}

fn print_nat_fn() -> NativeFunction {
    NativeFunction {
        name: "print".into(),
        func: |interp: &mut Interpreter, args: Vec<Value>| {
            if args.len() != 1 {
                let message = format!(
                    "invalid number of parameters function call. expected 1, got {}",
                    args.len()
                );
                panic!(message);
            }

            let value = args[0].to_ref();
            let item = interp.heap.load(value.address);
            let item_readable = item.read().unwrap();
            match item_readable.deref() {
                Item::ObjectInstance(ObjectInstance { ty: }) => {
                    println!("{}", s);
                    None
                }
                sym => panic!("expected an instance of String but got: {:?}", sym),
            }
        },
    }
}

