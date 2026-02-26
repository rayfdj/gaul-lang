use super::value::{NativeFunction, Value};
use std::rc::Rc;

pub fn all_native_functions() -> Vec<(&'static str, Value)> {
    vec![
        ("println", native_println()),
        ("print", native_print()),
        ("read_stdin", native_read_stdin()),
        ("read_line", native_read_line()),
        ("format", native_format()),
        ("type_of", native_type_of()),
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

fn native_read_stdin() -> Value {
    Value::NativeFn(Rc::from(NativeFunction {
        name: Rc::from("read_stdin"),
        arity: Some(0),
        func: |_args| {
            use std::io::Read;
            let mut s = String::new();
            std::io::stdin()
                .lock()
                .read_to_string(&mut s)
                .map_err(|e| format!("read_stdin: {}", e))?;
            Ok(Value::Str(s.into()))
        },
    }))
}

fn native_read_line() -> Value {
    Value::NativeFn(Rc::from(NativeFunction {
        name: Rc::from("read_line"),
        arity: Some(0),
        func: |_args| {
            use std::io::BufRead;
            let mut line = String::new();
            std::io::stdin()
                .lock()
                .read_line(&mut line)
                .map_err(|e| format!("read_line: {}", e))?;
            // Strip trailing \r\n (Windows) or \n (Unix)
            if line.ends_with('\n') {
                line.pop();
                if line.ends_with('\r') {
                    line.pop();
                }
            }
            Ok(Value::Str(line.into()))
        },
    }))
}

fn native_format() -> Value {
    Value::NativeFn(Rc::from(NativeFunction {
        name: Rc::from("format"),
        arity: None,
        func: |args| {
            if args.is_empty() {
                return Err("format expects at least 1 argument (the template string)".into());
            }
            let template = match &args[0] {
                Value::Str(s) => s.clone(),
                _ => return Err("format: first argument must be a string".into()),
            };
            let fill_args = &args[1..];
            let parts: Vec<&str> = template.split("{}").collect();
            let placeholder_count = parts.len() - 1;
            if placeholder_count != fill_args.len() {
                return Err(format!(
                    "format: template has {} placeholder(s) but {} argument(s) given",
                    placeholder_count,
                    fill_args.len()
                ));
            }
            let mut result = String::new();
            for (i, part) in parts.iter().enumerate() {
                result.push_str(part);
                if i < fill_args.len() {
                    result.push_str(&format!("{}", fill_args[i]));
                }
            }
            Ok(Value::Str(result.into()))
        },
    }))
}

fn native_type_of() -> Value {
    Value::NativeFn(Rc::from(NativeFunction {
        name: Rc::from("type_of"),
        arity: Some(1),
        func: |args| {
            let type_name = match &args[0] {
                Value::Num(_) => "number",
                Value::Str(_) => "string",
                Value::Bool(_) => "bool",
                Value::Null => "null",
                Value::Array(_) => "array",
                Value::Map(_) => "map",
                Value::Range(_, _) => "range",
                Value::Fn(_) | Value::NativeFn(_) => "function",
            };
            Ok(Value::Str(Rc::from(type_name)))
        },
    }))
}
