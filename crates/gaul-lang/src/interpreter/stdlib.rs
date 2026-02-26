use super::value::{NativeFunction, Value};
use std::collections::HashMap;
use std::rc::Rc;

/// Returns the exports for a built-in stdlib module, or None if not a stdlib name.
pub fn get_stdlib_module(name: &str) -> Option<HashMap<String, Value>> {
    match name {
        "math" => Some(math_module()),
        "fs" => Some(fs_module()),
        "sys" => Some(sys_module()),
        _ => None,
    }
}

/// Returns (module_name, export_name) pairs for all stdlib exports. Used by the drift test.
pub fn all_stdlib_exports() -> Vec<(&'static str, &'static str)> {
    vec![
        // math
        ("math", "sin"),
        ("math", "cos"),
        ("math", "tan"),
        ("math", "asin"),
        ("math", "acos"),
        ("math", "atan"),
        ("math", "atan2"),
        ("math", "log"),
        ("math", "log2"),
        ("math", "log10"),
        ("math", "exp"),
        ("math", "min"),
        ("math", "max"),
        ("math", "PI"),
        ("math", "E"),
        // fs
        ("fs", "read_file"),
        ("fs", "write_file"),
        ("fs", "append_file"),
        ("fs", "file_exists"),
        // sys
        ("sys", "args"),
        ("sys", "exit"),
        ("sys", "clock"),
        ("sys", "env_get"),
        ("sys", "env_set"),
    ]
}

fn native(name: &str, arity: Option<usize>, func: fn(&[Value]) -> Result<Value, String>) -> Value {
    Value::NativeFn(Rc::from(NativeFunction {
        name: Rc::from(name),
        arity,
        func,
    }))
}

// ── math module ──────────────────────────────────────────────────────────────

fn math_module() -> HashMap<String, Value> {
    let mut m = HashMap::new();

    // constants
    m.insert("PI".into(), Value::Num(std::f64::consts::PI));
    m.insert("E".into(), Value::Num(std::f64::consts::E));

    // trig
    m.insert(
        "sin".into(),
        native("sin", Some(1), |args| match &args[0] {
            Value::Num(n) => Ok(Value::Num(n.sin())),
            _ => Err("sin expects a number".into()),
        }),
    );
    m.insert(
        "cos".into(),
        native("cos", Some(1), |args| match &args[0] {
            Value::Num(n) => Ok(Value::Num(n.cos())),
            _ => Err("cos expects a number".into()),
        }),
    );
    m.insert(
        "tan".into(),
        native("tan", Some(1), |args| match &args[0] {
            Value::Num(n) => Ok(Value::Num(n.tan())),
            _ => Err("tan expects a number".into()),
        }),
    );
    m.insert(
        "asin".into(),
        native("asin", Some(1), |args| match &args[0] {
            Value::Num(n) => Ok(Value::Num(n.asin())),
            _ => Err("asin expects a number".into()),
        }),
    );
    m.insert(
        "acos".into(),
        native("acos", Some(1), |args| match &args[0] {
            Value::Num(n) => Ok(Value::Num(n.acos())),
            _ => Err("acos expects a number".into()),
        }),
    );
    m.insert(
        "atan".into(),
        native("atan", Some(1), |args| match &args[0] {
            Value::Num(n) => Ok(Value::Num(n.atan())),
            _ => Err("atan expects a number".into()),
        }),
    );
    m.insert(
        "atan2".into(),
        native("atan2", Some(2), |args| match (&args[0], &args[1]) {
            (Value::Num(y), Value::Num(x)) => Ok(Value::Num(y.atan2(*x))),
            _ => Err("atan2 expects two numbers".into()),
        }),
    );

    // logarithmic / exponential
    m.insert(
        "log".into(),
        native("log", Some(1), |args| match &args[0] {
            Value::Num(n) => Ok(Value::Num(n.ln())),
            _ => Err("log expects a number".into()),
        }),
    );
    m.insert(
        "log2".into(),
        native("log2", Some(1), |args| match &args[0] {
            Value::Num(n) => Ok(Value::Num(n.log2())),
            _ => Err("log2 expects a number".into()),
        }),
    );
    m.insert(
        "log10".into(),
        native("log10", Some(1), |args| match &args[0] {
            Value::Num(n) => Ok(Value::Num(n.log10())),
            _ => Err("log10 expects a number".into()),
        }),
    );
    m.insert(
        "exp".into(),
        native("exp", Some(1), |args| match &args[0] {
            Value::Num(n) => Ok(Value::Num(n.exp())),
            _ => Err("exp expects a number".into()),
        }),
    );

    // min / max (moved from builtins)
    m.insert(
        "min".into(),
        native("min", Some(2), |args| match (&args[0], &args[1]) {
            (Value::Num(a), Value::Num(b)) => Ok(Value::Num(if a <= b { *a } else { *b })),
            _ => Err("min expects two numbers".into()),
        }),
    );
    m.insert(
        "max".into(),
        native("max", Some(2), |args| match (&args[0], &args[1]) {
            (Value::Num(a), Value::Num(b)) => Ok(Value::Num(if a >= b { *a } else { *b })),
            _ => Err("max expects two numbers".into()),
        }),
    );

    m
}

// ── fs module ────────────────────────────────────────────────────────────────

fn fs_module() -> HashMap<String, Value> {
    let mut m = HashMap::new();

    m.insert(
        "read_file".into(),
        native("read_file", Some(1), |args| match &args[0] {
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
        }),
    );

    m.insert(
        "write_file".into(),
        native("write_file", Some(2), |args| match (&args[0], &args[1]) {
            (Value::Str(path), Value::Str(content)) => {
                std::fs::write(path.as_ref(), content.as_ref())
                    .map(|_| Value::Null)
                    .map_err(|e| format!("write_file: {}", e))
            }
            _ => Err("write_file expects (string path, string content)".into()),
        }),
    );

    m.insert(
        "append_file".into(),
        native("append_file", Some(2), |args| match (&args[0], &args[1]) {
            (Value::Str(path), Value::Str(content)) => {
                use std::io::Write;
                let mut file = std::fs::OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(path.as_ref())
                    .map_err(|e| format!("append_file: {}", e))?;
                file.write_all(content.as_ref().as_bytes())
                    .map(|_| Value::Null)
                    .map_err(|e| format!("append_file: {}", e))
            }
            _ => Err("append_file expects (string path, string content)".into()),
        }),
    );

    m.insert(
        "file_exists".into(),
        native("file_exists", Some(1), |args| match &args[0] {
            Value::Str(path) => Ok(Value::Bool(std::path::Path::new(path.as_ref()).exists())),
            _ => Err("file_exists expects a string".into()),
        }),
    );

    m
}

// ── sys module ───────────────────────────────────────────────────────────────

fn sys_module() -> HashMap<String, Value> {
    let mut m = HashMap::new();

    m.insert(
        "args".into(),
        native("args", Some(0), |_args| {
            let args: Vec<Value> = std::env::args()
                .map(|a| Value::Str(Rc::from(a.as_str())))
                .collect();
            Ok(Value::Array(Rc::new(std::cell::RefCell::new(args))))
        }),
    );

    m.insert(
        "exit".into(),
        native("exit", Some(1), |args| match &args[0] {
            Value::Num(code) => std::process::exit(*code as i32),
            _ => Err("exit expects a number".into()),
        }),
    );

    m.insert(
        "clock".into(),
        native("clock", Some(0), |_args| {
            use std::time::{SystemTime, UNIX_EPOCH};
            let secs = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_secs_f64())
                .unwrap_or(0.0);
            Ok(Value::Num(secs))
        }),
    );

    m.insert(
        "env_get".into(),
        native("env_get", Some(1), |args| match &args[0] {
            Value::Str(name) => match std::env::var(name.as_ref()) {
                Ok(val) => Ok(Value::Str(Rc::from(val.as_str()))),
                Err(_) => Ok(Value::Null),
            },
            _ => Err("env_get expects a string".into()),
        }),
    );

    m.insert(
        "env_set".into(),
        native("env_set", Some(2), |args| {
            match (&args[0], &args[1]) {
                (Value::Str(name), Value::Str(value)) => {
                    // SAFETY: we only set env vars from single-threaded Gaul scripts
                    unsafe {
                        std::env::set_var(name.as_ref(), value.as_ref());
                    }
                    Ok(Value::Null)
                }
                _ => Err("env_set expects (string name, string value)".into()),
            }
        }),
    );

    m
}
