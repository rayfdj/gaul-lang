use crate::interpreter::value::{MapKey, Value};
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
        (Value::Str(s), "replace") => {
            let old = match args.first() {
                Some(Value::Str(s)) => &s[..],
                _ => return Err("replace expects a string to find".into()),
            };
            let new = match args.get(1) {
                Some(Value::Str(s)) => &s[..],
                _ => return Err("replace expects a replacement string".into()),
            };
            Ok(Value::Str(s[..].replace(old, new).into()))
        }
        (Value::Str(s), "index_of") => {
            let sub = match args.first() {
                Some(Value::Str(s)) => &s[..],
                _ => return Err("index_of expects a string".into()),
            };
            match s[..].find(sub) {
                Some(byte_pos) => {
                    let char_index = s[..byte_pos].chars().count();
                    Ok(Value::Num(char_index as f64))
                }
                None => Ok(Value::Num(-1.0)),
            }
        }
        (Value::Str(s), "to_upper") => Ok(Value::Str(s[..].to_uppercase().into())),
        (Value::Str(s), "to_lower") => Ok(Value::Str(s[..].to_lowercase().into())),
        (Value::Str(s), "repeat") => {
            let n = match args.first() {
                Some(Value::Num(n)) => *n as usize,
                _ => return Err("repeat expects a number".into()),
            };
            Ok(Value::Str(s[..].repeat(n).into()))
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
        (Value::Array(elements), "enumerate") => {
            let pairs: Vec<Value> = elements
                .borrow()
                .iter()
                .enumerate()
                .map(|(i, v)| {
                    Value::Array(Rc::new(RefCell::new(vec![Value::Num(i as f64), v.clone()])))
                })
                .collect();
            Ok(Value::Array(Rc::new(RefCell::new(pairs))))
        }
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
                return Ok(Value::Null);
            }
            let mut min_val = f64::INFINITY;
            for val in arr.iter() {
                match val {
                    Value::Num(n) => {
                        if *n < min_val {
                            min_val = *n
                        }
                    }
                    _ => return Err("min expects all elements to be numbers".into()),
                }
            }
            Ok(Value::Num(min_val))
        }
        (Value::Array(elements), "max") => {
            let arr = elements.borrow();
            if arr.is_empty() {
                return Ok(Value::Null);
            }
            let mut max_val = f64::NEG_INFINITY;
            for val in arr.iter() {
                match val {
                    Value::Num(n) => {
                        if *n > max_val {
                            max_val = *n
                        }
                    }
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
        (Value::Array(elements), "slice") => {
            let start = match args.first() {
                Some(Value::Num(n)) => *n as usize,
                _ => return Err("slice expects a start index".into()),
            };
            let arr = elements.borrow();
            let end = match args.get(1) {
                Some(Value::Num(n)) => (*n as usize).min(arr.len()),
                None => arr.len(),
                _ => return Err("slice end must be a number".into()),
            };
            let start = start.min(arr.len());
            let sliced: Vec<Value> = arr.get(start..end).unwrap_or_default().to_vec();
            Ok(Value::Array(Rc::new(RefCell::new(sliced))))
        }
        (Value::Array(elements), "index_of") => {
            let search_item = args.first().ok_or("index_of expects a value")?;
            let arr = elements.borrow();
            match arr.iter().position(|v| v == search_item) {
                Some(idx) => Ok(Value::Num(idx as f64)),
                None => Ok(Value::Num(-1.0)),
            }
        }
        (Value::Array(elements), "flatten") => {
            let arr = elements.borrow();
            let mut result = Vec::new();
            for item in arr.iter() {
                match item {
                    Value::Array(inner) => result.extend(inner.borrow().iter().cloned()),
                    other => result.push(other.clone()),
                }
            }
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        (Value::Array(elements), "zip") => {
            let other = match args.first() {
                Some(Value::Array(a)) => a,
                _ => return Err("zip expects an array".into()),
            };
            let arr = elements.borrow();
            let other_arr = other.borrow();
            let len = arr.len().min(other_arr.len());
            let pairs: Vec<Value> = (0..len)
                .map(|i| {
                    Value::Array(Rc::new(RefCell::new(vec![
                        arr[i].clone(),
                        other_arr[i].clone(),
                    ])))
                })
                .collect();
            Ok(Value::Array(Rc::new(RefCell::new(pairs))))
        }
        (Value::Array(elements), "chunk") => {
            let n = match args.first() {
                Some(Value::Num(n)) => *n as usize,
                _ => return Err("chunk expects a number".into()),
            };
            if n == 0 {
                return Err("chunk size must be greater than 0".into());
            }
            let arr = elements.borrow();
            let chunks: Vec<Value> = arr
                .chunks(n)
                .map(|c| Value::Array(Rc::new(RefCell::new(c.to_vec()))))
                .collect();
            Ok(Value::Array(Rc::new(RefCell::new(chunks))))
        }
        (Value::Array(elements), "take") => {
            let n = match args.first() {
                Some(Value::Num(n)) => (*n as usize).min(elements.borrow().len()),
                _ => return Err("take expects a number".into()),
            };
            let arr = elements.borrow();
            let taken: Vec<Value> = arr[..n].to_vec();
            Ok(Value::Array(Rc::new(RefCell::new(taken))))
        }
        (Value::Array(elements), "skip") => {
            let n = match args.first() {
                Some(Value::Num(n)) => (*n as usize).min(elements.borrow().len()),
                _ => return Err("skip expects a number".into()),
            };
            let arr = elements.borrow();
            let skipped: Vec<Value> = arr[n..].to_vec();
            Ok(Value::Array(Rc::new(RefCell::new(skipped))))
        }
        (Value::Array(elements), "insert") => {
            let idx = match args.first() {
                Some(Value::Num(n)) => *n as usize,
                _ => return Err("insert expects an index".into()),
            };
            let val = args.get(1).ok_or("insert expects a value")?.clone();
            let mut arr = elements.borrow_mut();
            if idx > arr.len() {
                return Err(format!(
                    "insert index {} out of bounds for array of length {}",
                    idx,
                    arr.len()
                ));
            }
            arr.insert(idx, val);
            Ok(Value::Null)
        }
        (Value::Array(elements), "swap") => {
            let i = match args.first() {
                Some(Value::Num(n)) => *n as usize,
                _ => return Err("swap expects two indices".into()),
            };
            let j = match args.get(1) {
                Some(Value::Num(n)) => *n as usize,
                _ => return Err("swap expects two indices".into()),
            };
            let mut arr = elements.borrow_mut();
            if i >= arr.len() || j >= arr.len() {
                return Err(format!(
                    "swap indices ({}, {}) out of bounds for array of length {}",
                    i,
                    j,
                    arr.len()
                ));
            }
            arr.swap(i, j);
            Ok(Value::Null)
        }

        // Map!
        (Value::Map(map), "get") => {
            let key_val = args.first().ok_or("get expects a key")?;
            let key = MapKey::from_value(key_val)?;
            Ok(map.borrow().get(&key).cloned().unwrap_or(Value::Null))
        }
        (Value::Map(map), "get_or") => {
            let key_val = args.first().ok_or("get_or expects a key")?;
            let default = args.get(1).ok_or("get_or expects a default value")?;
            let key = MapKey::from_value(key_val)?;
            Ok(map
                .borrow()
                .get(&key)
                .cloned()
                .unwrap_or_else(|| default.clone()))
        }
        (Value::Map(map), "set") => {
            let key_val = args.first().ok_or("set expects a key")?;
            let value = args.get(1).ok_or("set expects a value")?.clone();
            let key = MapKey::from_value(key_val)?;
            map.borrow_mut().insert(key, value);
            Ok(Value::Null)
        }
        (Value::Map(map), "has") => {
            let key_val = args.first().ok_or("has expects a key")?;
            let key = MapKey::from_value(key_val)?;
            Ok(Value::Bool(map.borrow().contains_key(&key)))
        }
        (Value::Map(map), "remove") => {
            let key_val = args.first().ok_or("remove expects a key")?;
            let key = MapKey::from_value(key_val)?;
            Ok(map.borrow_mut().remove(&key).unwrap_or(Value::Null))
        }
        (Value::Map(map), "keys") => {
            let keys: Vec<Value> = map.borrow().keys().map(|k| k.to_value()).collect();
            Ok(Value::Array(Rc::new(RefCell::new(keys))))
        }
        (Value::Map(map), "values") => {
            let values: Vec<Value> = map.borrow().values().cloned().collect();
            Ok(Value::Array(Rc::new(RefCell::new(values))))
        }
        (Value::Map(map), "entries") => {
            let entries: Vec<Value> = map
                .borrow()
                .iter()
                .map(|(k, v)| Value::Array(Rc::new(RefCell::new(vec![k.to_value(), v.clone()]))))
                .collect();
            Ok(Value::Array(Rc::new(RefCell::new(entries))))
        }
        (Value::Map(map), "len") => Ok(Value::Num(map.borrow().len() as f64)),
        (Value::Map(map), "is_empty") => Ok(Value::Bool(map.borrow().is_empty())),

        // Range
        (Value::Range(from, _until), "from") => Ok(Value::Num(*from)),
        (Value::Range(_from, until), "until") => Ok(Value::Num(*until)),
        (Value::Range(from, until), "to_array") => {
            let elements: Vec<Value> = ((*from as i64)..(*until as i64))
                .map(|i| Value::Num(i as f64))
                .collect();
            Ok(Value::Array(Rc::new(RefCell::new(elements))))
        }

        _ => Err(format!(
            "'{}' is not a valid method for object '{}'",
            name, receiver
        )),
    }
}
