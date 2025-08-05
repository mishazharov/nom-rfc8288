use std::ops::{RangeFrom, RangeTo};

use nom::{
    AsChar, IResult, InputIter, InputLength, Offset, Slice,
    branch::alt,
    character::streaming::char,
    character::streaming::satisfy,
    combinator::recognize,
    error::ParseError,
    multi::{fold_many0, many1_count},
    sequence::{delimited, preceded},
};

use crate::{is_qdtext, is_quoted_pair, is_tchar};

/// ```text
/// TCHAR = "!" / "#" / "$" / "%" / "&" / "'" / "*"
///       / "+" / "-" / "." / "^" / "_" / "`" / "|" / "~"
///       / DIGIT / ALPHA
/// ```
pub fn tchar<I, E>(input: I) -> IResult<I, char, E>
where
    I: InputIter + Slice<RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    satisfy(is_tchar)(input)
}

/// `TOKEN = 1*TCHAR`
pub fn token<I, E>(input: I) -> IResult<I, I, E>
where
    I: InputIter + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>> + Copy + InputLength + Offset,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    recognize(many1_count(tchar))(input)
}

/// `QDTEXT = HTAB / SP / %x21 / %x23-5B / %x5D-7E / obs-text`
pub fn qdtext<I, E>(input: I) -> IResult<I, char, E>
where
    I: InputIter + Slice<RangeFrom<usize>> + Copy,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    satisfy(is_qdtext)(input)
}

/// `QUOTED-PAIR = "\" ( HTAB / SP / VCHAR / obs-text )`
fn quoted_pair<I, E>(input: I) -> IResult<I, char, E>
where
    I: InputIter + Slice<RangeFrom<usize>> + Copy + InputLength,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    preceded(char('\\'), satisfy(is_quoted_pair))(input)
}

/// `QUOTED-STRING = DQUOTE *( qdtext / quoted-pair ) DQUOTE`
pub fn quoted_string<I, E>(input: I) -> IResult<I, String, E>
where
    I: InputIter + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>> + Copy + InputLength + Offset,
    <I as InputIter>::Item: AsChar,
    E: ParseError<I>,
{
    delimited(
        char('"'),
        fold_many0(
            alt((qdtext, quoted_pair)),
            String::default,
            |mut collection, input| {
                collection.push(input);
                collection
            },
        ),
        char('"'),
    )(input)
}

#[cfg(test)]
mod tests {
    use nom::{Err as OutCome, Needed, error::VerboseError};

    use crate::streaming::{quoted_string, tchar, token};

    #[test]
    fn test_tchar() {
        assert_eq!(
            tchar::<_, VerboseError<&str>>(""),
            Err(OutCome::Incomplete(Needed::Unknown))
        );
        assert_eq!(tchar::<_, VerboseError<&str>>("mbbb"), Ok(("bbb", 'm')));
        assert_eq!(tchar::<_, VerboseError<&str>>("!aa"), Ok(("aa", '!')));
        assert!(matches!(
            tchar::<_, VerboseError<&str>>(","),
            Err(OutCome::Error(_))
        ));
    }

    #[test]
    fn test_token() {
        assert!(matches!(
            token::<_, VerboseError<&str>>(""),
            Err(OutCome::Incomplete(Needed::Unknown))
        ));
        assert_eq!(
            token::<_, VerboseError<&str>>("mbbb"),
            Err(OutCome::Incomplete(Needed::Unknown))
        );
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
            Ok(("", String::from(r#""#)))
        );

        assert_eq!(
            quoted_string::<_, VerboseError<&str>>(r#""hello""#),
            Ok(("", String::from(r#"hello"#)))
        );

        assert_eq!(
            quoted_string::<_, VerboseError<&str>>(r#""\"hello""#),
            Ok(("", String::from(r#""hello"#)))
        );

        assert!(matches!(
            quoted_string::<_, VerboseError<&str>>(r#""awd"#),
            Err(OutCome::Incomplete(Needed::Unknown))
        ));

        assert!(matches!(
            quoted_string::<_, VerboseError<&str>>(r#" "text""#),
            Err(OutCome::Error(_))
        ));

        assert_eq!(
            quoted_string::<_, VerboseError<&str>>(r#""awd"trailing"#),
            Ok(("trailing", String::from("awd")))
        );
    }
}
