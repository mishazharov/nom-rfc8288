use std::ops::{RangeFrom, RangeTo};

use itertools::Itertools;
use nom::{
    branch::alt,
    character::complete::{char, none_of, satisfy, space0},
    combinator::{all_consuming, opt, recognize},
    error::{ParseError, VerboseError},
    multi::{fold_many0, many0, many0_count, many1_count, many_m_n, separated_list1},
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
    recognize(delimited(
        char('"'),
        recognize(many0_count(alt((quoted_pair, qdtext)))),
        char('"'),
    ))(input)
}

fn quoted_string_alloca<I, E>(input: I) -> IResult<I, String, E>
where
    I: InputIter + Slice<RangeFrom<usize>> + Copy + InputLength,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    all_consuming(delimited(
        char('"'),
        fold_many0(alt((quoted_pair, qdtext)), String::new, |mut acc, item| {
            acc.push(item);
            acc
        }),
        char('"'),
    ))(input)
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

#[derive(PartialEq, Debug)]
pub struct LinkParam<'a> {
    pub key: &'a str,
    // This has to be a String because we need to strip out quotes and slashes
    // necessitating an allocation. We could probably use an Enum, and return
    // an &str when allocation isn't needed, but at this point the ease of use
    // is probably more important
    pub val: Option<String>,
}

#[derive(PartialEq, Debug)]
pub struct LinkData<'a> {
    pub url: &'a str,
    pub params: Vec<LinkParam<'a>>,
}

// https://datatracker.ietf.org/doc/html/rfc9110#name-recipient-requirements states that we should deal with at least some null elements
// and in fact the parser failed on the very first case I tried it on because of a trailing comma
const NUM_EMPTY_ELEMENTS: usize = 2;

pub fn link<'a, E>(
    input: &'a str,
) -> Result<Vec<Option<LinkData<'a>>>, nom::Err<VerboseError<&'a str>>>
where
    E: ParseError<&'a str>,
    nom::Err<VerboseError<&'a str>>: From<nom::Err<E>>,
{
    type ParserOutput<'s> = (
        &'s str,
        Vec<Option<(&'s str, Vec<(&'s str, Option<&'s str>)>)>>,
    );
    let (remainder, mut output): ParserOutput<'a> = list::<_, _, VerboseError<&str>, _>(
        NUM_EMPTY_ELEMENTS,
        tuple((
            delimited(char('<'), recognize(many0_count(none_of(">"))), char('>')),
            many0(preceded(
                tuple((space0, char(';'), space0)),
                pair(
                    token::<&str, VerboseError<&str>>,
                    opt(preceded(
                        pair(char('='), space0),
                        alt((quoted_string, token)),
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

    let res = output
        .drain(..)
        .map(|parsed_link| {
            let mut parsed_link = match parsed_link {
                Some(l) => l,
                None => return Ok(None),
            };

            let link_params = parsed_link
                .1
                .drain(..)
                .map(|link_param| {
                    let parsed_link_param_val = match link_param.1 {
                        None => None,
                        Some(link_param_val) if link_param_val.starts_with('"') => {
                            match quoted_string_alloca::<&str, E>(link_param_val) {
                                Ok(s) => Some(s.1),
                                Err(e) => return Err(e),
                            }
                        }
                        Some(link_param_val) => Some(link_param_val.to_owned()),
                    };

                    Ok(LinkParam {
                        key: link_param.0,
                        val: parsed_link_param_val,
                    })
                })
                .fold_ok(Vec::new(), |mut acc, item| {
                    acc.push(item);
                    acc
                })?;

            Ok(Some(LinkData {
                url: parsed_link.0,
                params: link_params,
            }))
        })
        .fold_ok(Vec::new(), |mut acc, item| {
            acc.push(item);
            acc
        });

    res
}

#[cfg(test)]
mod tests {
    use nom::{error::VerboseError, Err as OutCome};

    use crate::complete::{quoted_string, quoted_string_alloca, tchar, token, LinkData, LinkParam};

    use super::{link, list, quoted_pair};

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
            Ok(("", r#""""#))
        );

        assert_eq!(
            quoted_string::<_, VerboseError<&str>>(r#""hello""#),
            Ok(("", r#""hello""#))
        );

        assert_eq!(
            quoted_string::<_, VerboseError<&str>>(r#""\"hello""#),
            Ok(("", r#""\"hello""#))
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
            Ok(("trailing", r#""awd""#))
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

    #[test]
    fn test_link() {
        let input = r##"</terms>; rel="copyright"; anchor="#foo""##;

        let res = link::<VerboseError<&str>>(input).unwrap();

        assert_eq!(
            res,
            vec![Some(LinkData {
                url: "/terms",
                params: vec![
                    LinkParam {
                        key: "rel",
                        val: Some("copyright".into())
                    },
                    LinkParam {
                        key: "anchor",
                        val: Some("#foo".into())
                    }
                ]
            })]
        );
    }

    #[test]
    fn test_quoted_pair() {
        let input = r#"\a"#;

        let res = quoted_pair::<_, VerboseError<&str>>(input).unwrap();
        assert_eq!(res.1, 'a');
    }

    #[test]
    fn test_quoted_string_alloca() {
        let input = r#""aaaa""#;

        let res = quoted_string_alloca::<_, VerboseError<&str>>(input).unwrap();
        assert_eq!(res.1, "aaaa".to_owned());
    }

    #[test]
    fn test_quoted_string_alloca_quotes() {
        let input = r#""aa\"aa""#;

        let res = quoted_string_alloca::<_, VerboseError<&str>>(input).unwrap();
        assert_eq!(res.1, "aa\"aa".to_owned());
    }

    #[test]
    fn test_link_quoted_link_param() {
        let input = r##"</terms>; rel="copy\"right"; anchor=#foo"##;

        let res = link::<VerboseError<&str>>(input).unwrap();

        assert_eq!(
            res,
            vec![Some(LinkData {
                url: "/terms",
                params: vec![
                    LinkParam {
                        key: "rel",
                        val: Some("copy\"right".into())
                    },
                    LinkParam {
                        key: "anchor",
                        val: Some("#foo".into())
                    }
                ]
            })]
        );
    }
}
