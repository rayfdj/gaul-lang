use super::value::{NativeFunction, Value};
use std::rc::Rc;

pub fn all_native_functions() -> Vec<(&'static str, Value)> {
    vec![
        ("println", native_println()),
        ("print", native_print()),
        ("read_file", native_read_file()),
        ("min", native_min()),
        ("max", native_max()),
    ]
}

fn native_println() -> Value {
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

fn native_print() -> Value {
    Value::NativeFn(Rc::from(NativeFunction {
        name: Rc::from("print"),
        arity: None,
        func: |args| {
            use std::io::Write;
            for arg in args {
                print!("{}", arg);
            }
            let _ = std::io::stdout().flush();
            Ok(Value::Null)
        },
    }))
}

fn native_min() -> Value {
    Value::NativeFn(Rc::from(NativeFunction {
        name: Rc::from("min"),
        arity: Some(2),
        func: |args| match (&args[0], &args[1]) {
            (Value::Num(a), Value::Num(b)) => Ok(Value::Num(if a <= b { *a } else { *b })),
            _ => Err("min expects two numbers".into()),
        },
    }))
}

fn native_max() -> Value {
    Value::NativeFn(Rc::from(NativeFunction {
        name: Rc::from("max"),
        arity: Some(2),
        func: |args| match (&args[0], &args[1]) {
            (Value::Num(a), Value::Num(b)) => Ok(Value::Num(if a >= b { *a } else { *b })),
            _ => Err("max expects two numbers".into()),
        },
    }))
}

fn native_read_file() -> Value {
    Value::NativeFn(Rc::from(NativeFunction {
        name: Rc::from("read_file"),
        arity: Some(1),
        func: |args| match &args[0] {
            Value::Str(path) => std::fs::read_to_string(path.as_ref())
                .map(|s| Value::Str(s.into()))
                .map_err(|e| {
                    let cwd = std::env::current_dir()
                        .unwrap_or_else(|_| std::path::PathBuf::from("unknown"));
                    let absolute_path = cwd.join(path.as_ref());
                    format!(
                        "Error reading '{}'\n  -> Resolved to: '{}'\n  -> System Error: {}",
                        path,
                        absolute_path.display(),
                        e
                    )
                }),
            _ => Err("read_file expects a string".into()),
        },
    }))
}
