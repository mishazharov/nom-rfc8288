use nom::{error::ParseError, AsChar, IResult, Parser};

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

/// QDTEXT = HTAB / SP / %x21 / %x23-5B / %x5D-7E / obs-text
pub fn is_qdtext(c: impl AsChar) -> bool {
    matches!(c.as_char(), '\t' | ' ' | '\x21' | '\x23' ..= '\x5B' | '\x5D' ..= '\x7E' | '\u{80}' ..= '\u{FF}')
}

/// Only implements the ( HTAB / SP / VCHAR / obs-text ) component of QUOTED-PAIR
fn is_quoted_pair(c: impl AsChar) -> bool {
    matches!(c.as_char(), '\t' | ' ' | '\x21' ..= '\x7E' | '\u{80}' ..= '\u{FF}')
}

/// If the condition is true, this parser only returns `Some` or `None`
fn optional_parser<I, O, E, L>(
    condition: bool,
    mut element: L,
) -> impl FnMut(I) -> IResult<I, Option<O>, E>
where
    L: Parser<I, O, E>,
    E: ParseError<I>,
    I: Copy,
{
    move |input: I| {
        let res = element.parse(input);

        if condition {
            return Ok(match res {
                Ok((k, j)) => (k, Some(j)),
                Err(_) => (input, None),
            });
        }

        match res {
            Ok((k, j)) => Ok((k, Some(j))),
            Err(e) => Err(e),
        }
    }
}
