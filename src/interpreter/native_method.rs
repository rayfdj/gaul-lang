use crate::interpreter::value::Value;

pub fn call_native_method(receiver: &Value, name: &str, args: &[Value]) -> Result<Value, String> {
    match (receiver, name) {
        // String methods!
        (Value::Str(s), "len") => Ok(Value::Num(s.chars().count() as f64)),
        (Value::Str(s), "char_at") => {
            let idx = match args.get(0) {
                Some(Value::Num(n)) => *n as usize,
                _ => return Err("char_at expects a number".into()),
            };
            match s.chars().nth(idx) {
                Some(c) => Ok(Value::Str(c.to_string().into())),
                None => Err(format!("index {} out of bounds", idx).into()),
            }
        }
        (Value::Str(s), "substring") => {
            let start = match args.get(0) {
                Some(Value::Num(n)) => *n as usize,
                _ => return Err("substring expects a start index".into()),
            };

            let chars: Vec<char> = s.chars().collect();

            let end = match args.get(1) {
                Some(Value::Num(n)) => *n as usize,
                None => chars.len(),
                _ => {
                    return Err(format!(
                        "unexpected error in 'substring' with args: '{:?}'",
                        args
                    ));
                }
            };

            let result: String = chars
                .get(start..end)
                .map(|slice| slice.iter().collect())
                .unwrap_or_default();

            Ok(Value::Str(result.into()))
        }
        (Value::Str(s), "to_num") => Ok(s
            .parse::<f64>()
            .map(Value::Num)
            .map_err(|err| err.to_string())?),

        // Number methods!
        (Value::Num(n), "to_str") => Ok(Value::Str(n.to_string().into())),
        (Value::Num(n), "abs") => Ok(Value::Num(n.abs())),
        (Value::Num(n), "floor") => Ok(Value::Num(n.floor())),
        (Value::Num(n), "ceil") => Ok(Value::Num(n.ceil())),
        (Value::Num(n), "round") => Ok(Value::Num(n.round())),
        (Value::Num(n), "pow") => {
            let exponent = match args.get(0) {
                Some(Value::Num(n)) => *n,
                _ => return Err("pow expects an exponent".into()),
            };
            Ok(Value::Num(n.powf(exponent)))
        }
        (Value::Num(n), "sqrt") => {
            if *n < 0.0 {
                Err("sqrt expects a non-negative number".into())
            } else {
                Ok(Value::Num(n.sqrt()))
            }
        }

        // Bool? Not much to do...
        (Value::Bool(b), "to_str") => Ok(Value::Str(b.to_string().into())),

        _ => Err(format!("'{}' is not a valid method for object '{}'", name, receiver).into()),
    }
}
