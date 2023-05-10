use std::ops::{RangeFrom, RangeTo};

use nom::{
    character::complete::satisfy, combinator::recognize, error::ParseError, multi::many1_count,
    AsChar, IResult, InputIter, InputLength, Offset, Slice,
};

use crate::is_tchar;

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

#[cfg(test)]
mod tests {
    use nom::{error::VerboseError, Err as OutCome, Needed};

    use crate::complete::{tchar, token};

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
}
