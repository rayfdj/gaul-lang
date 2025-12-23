use std::rc::Rc;
use super::value::{Value, NativeFunction};

pub fn all_native_functions() -> Vec<(&'static str, Value)> {
    vec![
        ("println", println()),
        ("read_file", read_file()),
    ]
}

fn println() -> Value {
    Value::NativeFn(Rc::from(NativeFunction {
        name: Rc::from("println"),
        arity: None,
        func: |args| {
            for arg in args {
                print!("{}", arg);
            }
            println!();
            Ok(Value::Null)
        },
    }))
}

fn read_file() -> Value {
    Value::NativeFn(Rc::from(NativeFunction {
        name: Rc::from("read_file"),
        arity: Some(1),
        func: |args| {
            match &args[0] {
                Value::Str(path) => {
                    std::fs::read_to_string(path.as_ref())
                        .map(|s| Value::Str(s.into()))
                        .map_err(|e| {
                            let cwd = std::env::current_dir().unwrap_or_else(|_| std::path::PathBuf::from("unknown"));
                            let absolute_path = cwd.join(path.as_ref());
                            format!(
                                "Error reading '{}'\n  -> Resolved to: '{}'\n  -> System Error: {}",
                                path,
                                absolute_path.display(),
                                e
                            )
                        })
                }
                _ => Err("read_file expects a string".into()),
            }
        },
    }))
}