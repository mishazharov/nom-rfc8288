use std::{
    collections::HashMap,
    ops::{RangeFrom, RangeTo},
};

use nom::{
    branch::alt,
    character::complete::{char, none_of, satisfy, space0},
    combinator::{opt, recognize},
    error::{ParseError, VerboseError},
    multi::{many0, many0_count, many1_count, many_m_n, separated_list1},
    sequence::{delimited, pair, preceded, tuple},
    AsChar, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, Parser, Slice,
};

use crate::{is_qdtext, is_quoted_pair, is_tchar, optional_parser};

/// TCHAR = "!" / "#" / "$" / "%" / "&" / "'" / "*"
///       / "+" / "-" / "." / "^" / "_" / "`" / "|" / "~"
///       / DIGIT / ALPHA
pub fn tchar<I, E>(input: I) -> IResult<I, char, E>
where
    I: InputIter + Slice<RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    satisfy(is_tchar)(input)
}

/// TOKEN = 1*TCHAR
pub fn token<I, E>(input: I) -> IResult<I, I, E>
where
    I: InputIter + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>> + Copy + InputLength + Offset,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    recognize(many1_count(tchar))(input)
}

/// QDTEXT = HTAB / SP / %x21 / %x23-5B / %x5D-7E / obs-text
pub fn qdtext<I, E>(input: I) -> IResult<I, char, E>
where
    I: InputIter + Slice<RangeFrom<usize>> + Copy,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    satisfy(is_qdtext)(input)
}

/// QUOTED-PAIR = "\" ( HTAB / SP / VCHAR / obs-text )
fn quoted_pair<I, E>(input: I) -> IResult<I, char, E>
where
    I: InputIter + Slice<RangeFrom<usize>> + Copy,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    preceded(char('\\'), satisfy(is_quoted_pair))(input)
}

/// QUOTED-STRING = DQUOTE *( qdtext / quoted-pair ) DQUOTE
///
/// If the parser succeeds, we return the unmodified string (with backslashes included)
/// to prevent allocation and to make sure that all of the return types are consistent
/// when using nom combinators
fn quoted_string<I, E>(input: I) -> IResult<I, I, E>
where
    I: InputIter + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>> + Copy + InputLength + Offset,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    delimited(
        char('"'),
        recognize(many0_count(alt((qdtext, quoted_pair)))),
        char('"'),
    )(input)
}

/// #rule
/// #element => [ element ] *( OWS "," OWS [ element ] )
///
/// RFC 9110 specifies that:
///
/// > A recipient MUST parse and ignore a reasonable number of empty list
/// > elements: enough to handle common mistakes by senders that merge
/// > values, but not so much that they could be used as a denial-of-
/// > service mechanism.
///
/// However, the RFC does not specify what a "reasonable" value for this is,
/// so we allow the user to configure such a limit.
///
/// In this implementation, empty list elements are represented as `None`
/// in the returned `Vec`, and non-empty list elements are represented as
/// `Some<E>`
pub fn list<I, O, E, L>(
    reasonable_count: usize,
    element: L,
    input: I,
) -> IResult<I, Vec<Option<O>>, E>
where
    L: Parser<I, O, E>,
    E: ParseError<I>,
    I: InputLength
        + Clone
        + Copy
        + InputTakeAtPosition
        + InputIter
        + InputTake
        + Slice<RangeFrom<usize>>,
    <I as InputIter>::Item: Clone + AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    let allow_empty_elements = reasonable_count != 0;
    separated_list1(
        many_m_n(1, reasonable_count + 1, tuple((space0, char(','), space0))),
        optional_parser(allow_empty_elements, element),
    )(input)
}

pub struct LinkData<'a> {
    url: &'a str,
    params: HashMap<&'a str, &'a str>,
}

pub fn link<'a, E>(input: &str) -> Result<Vec<LinkData<'a>>, nom::Err<VerboseError<&str>>>
where
{
    let (remainder, output): (_, Vec<Option<(&str, Vec<(&str, Option<&str>)>)>>) =
        list::<_, _, VerboseError<&str>, _>(
            0,
            tuple((
                delimited(char('<'), recognize(many0_count(none_of(">"))), char('>')),
                many0(preceded(
                    tuple((space0, char(';'), space0)),
                    pair(
                        token::<&str, VerboseError<&str>>,
                        opt(preceded(
                            pair(char('='), space0),
                            alt((token, quoted_string)),
                        )),
                    ),
                )),
            )),
            input,
        )?;

    if !remainder.is_empty() {
        return Err(nom::Err::Error(VerboseError::from_char(
            remainder,
            remainder.chars().next().unwrap(),
        )));
    }

    todo!();
}

#[cfg(test)]
mod tests {
    use nom::{error::VerboseError, Err as OutCome};

    use crate::complete::{quoted_string, tchar, token};

    use super::list;

    #[test]
    fn test_tchar() {
        assert_eq!(tchar::<_, VerboseError<&str>>("mbbb"), Ok(("bbb", 'm')));
        assert_eq!(tchar::<_, VerboseError<&str>>("!aa"), Ok(("aa", '!')));
        assert!(tchar::<_, VerboseError<&str>>(",").is_err());
    }

    #[test]
    fn test_token() {
        assert!(matches!(
            token::<_, VerboseError<&str>>(""),
            Err(OutCome::Error(_))
        ));
        assert_eq!(token::<_, VerboseError<&str>>("mbbb"), Ok(("", "mbbb")));
        assert_eq!(token::<_, VerboseError<&str>>("a,"), Ok((",", "a")));
        assert!(matches!(
            token::<_, VerboseError<&str>>(","),
            Err(OutCome::Error(_))
        ));
    }

    #[test]
    fn test_quoted_string() {
        assert_eq!(
            quoted_string::<_, VerboseError<&str>>(r#""""#),
            Ok(("", r#""#))
        );

        assert_eq!(
            quoted_string::<_, VerboseError<&str>>(r#""hello""#),
            Ok(("", r#"hello"#))
        );

        assert_eq!(
            quoted_string::<_, VerboseError<&str>>(r#""\"hello""#),
            Ok(("", r#"\"hello"#))
        );

        assert!(matches!(
            quoted_string::<_, VerboseError<&str>>(r#""awd"#),
            Err(OutCome::Error(_))
        ));

        assert!(matches!(
            quoted_string::<_, VerboseError<&str>>(r#" "text""#),
            Err(OutCome::Error(_))
        ));

        assert_eq!(
            quoted_string::<_, VerboseError<&str>>(r#""awd"trailing"#),
            Ok(("trailing", "awd"))
        );
    }

    #[test]
    fn test_list_rule() {
        assert_eq!(
            list::<_, _, VerboseError<&str>, _>(0, token, "a,b,c"),
            Ok(("", vec![Some("a"), Some("b"), Some("c")]))
        );

        assert_eq!(
            list::<_, _, VerboseError<&str>, _>(0, token, "a , b , c"),
            Ok(("", vec![Some("a"), Some("b"), Some("c")]))
        );

        assert_eq!(
            list::<_, _, VerboseError<&str>, _>(0, token, "a , b , "),
            Ok((" , ", vec![Some("a"), Some("b")]))
        );

        assert_eq!(
            list::<_, _, VerboseError<&str>, _>(0, token, "a , b , ,"),
            Ok((" , ,", vec![Some("a"), Some("b")]))
        );
    }
}
