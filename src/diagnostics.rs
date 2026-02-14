use crate::span::Span;

pub fn render(source: &str, kind: &str, span: Span, message: &str, hint: Option<&str>) -> String {
    let lines: Vec<&str> = source.lines().collect();
    let line_idx = span.line.saturating_sub(1);
    let source_line = lines.get(line_idx).unwrap_or(&"");

    let line_num = span.line.to_string();
    let gutter_width = line_num.len();

    let pointer_col = span.col.saturating_sub(1);
    let pointer_len = span.length.max(1);

    let mut out = String::new();

    // error[kind]: message
    out.push_str(&format!("error[{}]: {}\n", kind, message));

    // --> line:col
    out.push_str(&format!(
        "{:>width$}--> line {}:{}\n",
        " ",
        span.line,
        span.col,
        width = gutter_width
    ));

    // empty gutter line
    out.push_str(&format!("{:>width$} |\n", " ", width = gutter_width));

    // source line
    out.push_str(&format!(
        "{:>width$} | {}\n",
        span.line,
        source_line,
        width = gutter_width
    ));

    // pointer line
    let padding: String = source_line
        .chars()
        .take(pointer_col)
        .map(|c| if c == '\t' { '\t' } else { ' ' })
        .collect();
    let carets = "^".repeat(pointer_len);
    out.push_str(&format!(
        "{:>width$} | {}{}\n",
        " ",
        padding,
        carets,
        width = gutter_width
    ));

    // hint
    if let Some(hint) = hint {
        out.push_str(&format!(
            "{:>width$} |\n",
            " ",
            width = gutter_width
        ));
        out.push_str(&format!(
            "{:>width$} = hint: {}\n",
            " ",
            hint,
            width = gutter_width
        ));
    }

    out
}

pub fn suggest_hint(message: &str) -> Option<String> {
    let msg = message.to_lowercase();

    if msg.contains("cannot apply") && msg.contains("plus") {
        if (msg.contains("num") && msg.contains("str"))
            || (msg.contains("str") && msg.contains("num"))
        {
            return Some("use .to_str() to convert the number first".into());
        }
    }

    if msg.contains("cannot assign to immutable variable")
        || msg.contains("immutable")
    {
        return Some("declare with 'var' instead of 'let' to make it mutable".into());
    }

    if msg.contains("condition must") && msg.contains("bool") {
        return Some("use == to compare values".into());
    }

    if msg.contains("is not callable") {
        return Some("make sure the function is defined before this line".into());
    }

    None
}
