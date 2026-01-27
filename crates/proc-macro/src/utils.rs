use codama_nodes::CamelCaseString;

pub fn to_snake_case(s: &CamelCaseString) -> String {
    let mut out = String::with_capacity(s.len());
    for (i, ch) in s.chars().enumerate() {
        if ch.is_uppercase() && i != 0 {
            out.push('_');
        }
        out.push(ch.to_ascii_lowercase());
    }
    out
}

pub fn to_pascal_case(s: &CamelCaseString) -> String {
    let s = s.as_str();
    if let Some((first, rest)) = s.chars().next().map(|first| (first, &s[1..])) {
        let mut result = String::with_capacity(s.len());
        result.push(first.to_ascii_uppercase());
        result.push_str(rest);
        result
    } else {
        String::new()
    }
}

pub fn to_snake_case_str(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for (i, ch) in s.chars().enumerate() {
        if ch.is_uppercase() && i != 0 {
            out.push('_');
        }
        out.push(ch.to_ascii_lowercase());
    }
    out
}

pub fn to_pascal_case_str(s: &str) -> String {
    if let Some((first, rest)) = s.chars().next().map(|first| (first, &s[1..])) {
        let mut result = String::with_capacity(s.len());
        result.push(first.to_ascii_uppercase());
        result.push_str(rest);
        result
    } else {
        String::new()
    }
}
