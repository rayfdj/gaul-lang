use crate::interpreter::value::Value;
use std::cell::RefCell;
use std::rc::Rc;

pub fn call_native_method(receiver: &Value, name: &str, args: &[Value]) -> Result<Value, String> {
    match (receiver, name) {
        // String methods!
        (Value::Str(s), "len") => Ok(Value::Num(s.chars().count() as f64)),
        (Value::Str(s), "char_at") => {
            let idx = match args.first() {
                Some(Value::Num(n)) => *n as usize,
                _ => return Err("char_at expects a number".into()),
            };
            match s.chars().nth(idx) {
                Some(c) => Ok(Value::Str(c.to_string().into())),
                None => Err(format!("index {} out of bounds", idx)),
            }
        }
        (Value::Str(s), "substring") => {
            let start = match args.first() {
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
            let delimiter = match args.first() {
                Some(Value::Str(d)) => &d[..],
                _ => return Err("split expects a string delimiter".into()),
            };

            let parts: Vec<Value> = s[..]
                .split(delimiter)
                .map(|part| Value::Str(part.to_string().into()))
                .collect();

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
            let sub = match args.first() {
                Some(Value::Str(s)) => &s[..],
                _ => return Err("contains expects a string".into()),
            };
            Ok(Value::Bool(s[..].contains(sub)))
        }
        (Value::Str(s), "starts_with") => {
            let sub = match args.first() {
                Some(Value::Str(s)) => &s[..],
                _ => return Err("starts_with expects a string".into()),
            };
            Ok(Value::Bool(s[..].starts_with(sub)))
        }
        (Value::Str(s), "ends_with") => {
            let sub = match args.first() {
                Some(Value::Str(s)) => &s[..],
                _ => return Err("ends_with expects a string".into()),
            };
            Ok(Value::Bool(s[..].ends_with(sub)))
        }
        (Value::Str(s), "to_num") => Ok(s
            .parse::<f64>()
            .map(Value::Num)
            .map_err(|err| err.to_string())?),
        (Value::Str(s), "chars") => {
            let chars: Vec<Value> = s
                .chars()
                .map(|c| Value::Str(c.to_string().into()))
                .collect();
            Ok(Value::Array(Rc::new(RefCell::new(chars))))
        }

        // Number methods!
        (Value::Num(n), "to_str") => Ok(Value::Str(n.to_string().into())),
        (Value::Num(n), "abs") => Ok(Value::Num(n.abs())),
        (Value::Num(n), "floor") => Ok(Value::Num(n.floor())),
        (Value::Num(n), "ceil") => Ok(Value::Num(n.ceil())),
        (Value::Num(n), "round") => Ok(Value::Num(n.round())),
        (Value::Num(n), "pow") => {
            let exponent = match args.first() {
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
            let divisor = match args.first() {
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
            let divisor = match args.first() {
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
            let idx = match args.first() {
                Some(Value::Num(n)) => *n as usize,
                _ => return Err("get expects a number".into()),
            };
            elements
                .borrow()
                .get(idx)
                .cloned()
                .ok_or_else(|| format!("index {} out of bounds", idx))
        }
        (Value::Array(elements), "first") => {
            Ok(elements.borrow().first().cloned().unwrap_or(Value::Null))
        }
        (Value::Array(elements), "last") => {
            Ok(elements.borrow().last().cloned().unwrap_or(Value::Null))
        }
        (Value::Array(elements), "push") => {
            let val = args.first().ok_or("push expects a value")?.clone();
            elements.borrow_mut().push(val);
            Ok(Value::Null) // Returns null (like Rust/Python).
        }
        (Value::Array(elements), "pop") => {
            // Returns the popped value, or Null if empty
            Ok(elements.borrow_mut().pop().unwrap_or(Value::Null))
        }
        (Value::Array(elements), "set") => {
            let idx = match args.first() {
                Some(Value::Num(n)) => *n as usize,
                _ => return Err("set expects index as number".into()),
            };
            let val = args.get(1).ok_or("set expects a value")?.clone();

            let mut arr = elements.borrow_mut();
            if idx < arr.len() {
                arr[idx] = val; // Replaces the value at index
                Ok(Value::Null)
            } else {
                Err(format!("index {} out of bounds", idx))
            }
        }
        (Value::Array(elements), "remove") => {
            let idx = match args.first() {
                Some(Value::Num(n)) => *n as usize,
                _ => return Err("remove expects index as number".into()),
            };

            let mut arr = elements.borrow_mut();
            if idx < arr.len() {
                Ok(arr.remove(idx)) // Shifts everything after it to the left
            } else {
                Err(format!("index {} out of bounds", idx))
            }
        }
        (Value::Array(elements), "contains") => {
            let search_item = args.first().ok_or("contains expects a value")?;
            let arr = elements.borrow();
            // Values must implement PartialEq for this to work.
            let found = arr.contains(search_item);
            Ok(Value::Bool(found))
        }
        (Value::Array(elements), "reverse") => {
            let mut reversed = elements.borrow().clone();
            reversed.reverse();
            Ok(Value::Array(Rc::new(RefCell::new(reversed))))
        }
        (Value::Array(elements), "is_empty") => Ok(Value::Bool(elements.borrow().is_empty())),
        (Value::Array(elements), "join") => {
            let separator = match args.first() {
                Some(Value::Str(s)) => s.clone(),
                Some(_) => return Err("join expects a string separator".into()),
                None => "".into(), // Default to no separator
            };

            let arr = elements.borrow();
            let strings: Vec<String> = arr.iter().map(|v| v.to_string()).collect();
            Ok(Value::Str(strings.join(&separator).into()))
        }
        (Value::Array(elements), "sum") => {
            let arr = elements.borrow();
            let mut total = 0.0;
            for val in arr.iter() {
                match val {
                    Value::Num(n) => total += n,
                    _ => return Err("sum expects all elements to be numbers".into()),
                }
            }
            Ok(Value::Num(total))
        }
        (Value::Array(elements), "min") => {
            let arr = elements.borrow();
            if arr.is_empty() {
                return Ok(Value::Null)
            }
            let mut min_val = f64::INFINITY;
            for val in arr.iter() {
                match val {
                    Value::Num(n) => if *n < min_val { min_val = *n },
                    _ => return Err("min expects all elements to be numbers".into()),
                }
            }
            Ok(Value::Num(min_val))
        }
        (Value::Array(elements), "max") => {
            let arr = elements.borrow();
            if arr.is_empty() {
                return Ok(Value::Null)
            }
            let mut max_val = f64::NEG_INFINITY;
            for val in arr.iter() {
                match val {
                    Value::Num(n) => if *n > max_val { max_val = *n },
                    _ => return Err("max expects all elements to be numbers".into()),
                }
            }
            Ok(Value::Num(max_val))
        }
        (Value::Array(elements), "sort") => {
            let arr = elements.borrow();
            if arr.is_empty() {
                return Ok(Value::Array(Rc::new(RefCell::new(vec![]))));
            }

            // Check first element to determine type
            match arr.first() {
                Some(Value::Num(_)) => {
                    // Sort numbers
                    let mut nums: Vec<f64> = vec![];
                    for val in arr.iter() {
                        match val {
                            Value::Num(n) => nums.push(*n),
                            _ => return Err("sort expects all elements to be the same type".into()),
                        }
                    }
                    nums.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
                    let sorted: Vec<Value> = nums.into_iter().map(Value::Num).collect();
                    Ok(Value::Array(Rc::new(RefCell::new(sorted))))
                }
                Some(Value::Str(_)) => {
                    // Sort strings lexicographically
                    let mut strs: Vec<Rc<str>> = vec![];
                    for val in arr.iter() {
                        match val {
                            Value::Str(s) => strs.push(s.clone()),
                            _ => return Err("sort expects all elements to be the same type".into()),
                        }
                    }
                    strs.sort();
                    let sorted: Vec<Value> = strs.into_iter().map(Value::Str).collect();
                    Ok(Value::Array(Rc::new(RefCell::new(sorted))))
                }
                _ => Err("sort only supports arrays of numbers or strings".into()),
            }
        }

        // Range
        (Value::Range(from, _until), "from") => Ok(Value::Num(*from)),
        (Value::Range(_from, until), "until") => Ok(Value::Num(*until)),

        _ => Err(format!(
            "'{}' is not a valid method for object '{}'",
            name, receiver
        )),
    }
}
