use nom::AsChar;

pub mod complete;
pub mod streaming;

/// Token character
///
/// TCHAR = "!" / "#" / "$" / "%" / "&" / "'" / "*"
///       / "+" / "-" / "." / "^" / "_" / "`" / "|" / "~"
///       / DIGIT / ALPHA
pub fn is_tchar(c: impl AsChar) -> bool {
    // This is implemented as one match statement because to call
    // is_alpha and is_digit would impose the Copy trait on this
    // function call
    matches!(c.as_char(),
             '!' | '#' | '$' | '%' | '&' | '\'' | '*' |
             '+' | '-' | '.' | '^' | '_' | '`' | '|' | '~' |
             '\x41'..='\x5A' | '\x61'..='\x7A' | // A-Z, a-z
             '\x30'..='\x39' // Digits
    )
}
