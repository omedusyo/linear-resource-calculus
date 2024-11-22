use std::fmt;

pub fn fmt_sequence<A: fmt::Display>(xs: &[A], delimiter: &str, f: &mut fmt::Formatter) -> fmt::Result {
    match xs.len() {
        0 => write!(f, ""),
        1 => write!(f, "{}", xs[0]),
        _ => {
            write!(f, "{}", xs[0])?;
            for pattern in &xs[1..] {
                write!(f, "{} {}", delimiter, pattern)?;
            }
            Ok(())
        },
    }
}
