use crate::interpreter::value::Value;
use std::cell::RefCell;
use std::rc::Rc;

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
        (Value::Str(s), "split") => {
            let delimiter = match args.get(0) {
                Some(Value::Str(d)) => &d[..],
                _ => return Err("split expects a string delimiter".into()),
            };

            let parts: Vec<Value> = s[..]
                .split(delimiter)
                .map(|part| Value::Str(part.to_string().into()))
                .collect();

            // You'll need to wrap this in your array container type
            // Assuming: use std::rc::Rc; use std::cell::RefCell;
            Ok(Value::Array(Rc::new(RefCell::new(parts))))
        }
        (Value::Str(s), "lines") => {
            // .lines() in Rust handles \n and \r\n automatically!
            let lines: Vec<Value> = s[..]
                .lines()
                .map(|line| Value::Str(line.to_string().into()))
                .collect();

            Ok(Value::Array(Rc::new(RefCell::new(lines))))
        }
        (Value::Str(s), "trim") => Ok(Value::Str(s[..].trim().to_string().into())),
        (Value::Str(s), "contains") => {
            let sub = match args.get(0) {
                Some(Value::Str(s)) => &s[..],
                _ => return Err("contains expects a string".into()),
            };
            Ok(Value::Bool(s[..].contains(sub)))
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
        (Value::Num(n), "mod") => {
            let divisor = match args.get(0) {
                Some(Value::Num(d)) => *d,
                _ => return Err("mod expects a number".into()),
            };

            // Safety Check: Avoid NaN
            if divisor == 0.0 {
                return Err("modulo by zero".into());
            }

            Ok(Value::Num(n.rem_euclid(divisor)))
        }
        (Value::Num(n), "floor_div") => {
            let divisor = match args.get(0) {
                Some(Value::Num(d)) => *d,
                _ => return Err("floor_div expects a number".into()),
            };

            // Safety Check: Avoid NaN
            if divisor == 0.0 {
                return Err("floor division by zero".into());
            }

            Ok(Value::Num(n.div_euclid(divisor)))
        }

        // Bool? Not much to do...
        (Value::Bool(b), "to_str") => Ok(Value::Str(b.to_string().into())),

        // Array!
        (Value::Array(elements), "len") => Ok(Value::Num(elements.borrow().len() as f64)),
        (Value::Array(elements), "get") => {
            let idx = match args.get(0) {
                Some(Value::Num(n)) => *n as usize,
                _ => return Err("get expects a number".into()),
            };
            elements
                .borrow()
                .get(idx)
                .cloned()
                .ok_or_else(|| format!("index {} out of bounds", idx).into())
        }
        (Value::Array(elements), "first") => {
            Ok(elements.borrow().first().cloned().unwrap_or(Value::Null))
        }
        (Value::Array(elements), "last") => {
            Ok(elements.borrow().last().cloned().unwrap_or(Value::Null))
        }

        _ => Err(format!("'{}' is not a valid method for object '{}'", name, receiver).into()),
    }
}
