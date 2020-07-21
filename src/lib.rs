use bigdecimal::BigDecimal;
use linked_hash_map::LinkedHashMap;
use nom::branch::alt;
use nom::bytes::complete::{tag, take, take_while, take_while1};
use nom::character::complete::{char as onechar, one_of};
use nom::combinator::opt;
use nom::combinator::{all_consuming, cut};
use nom::error::{ErrorKind, ParseError, VerboseError};
use nom::IResult;

#[derive(Debug, Clone, PartialEq)]
pub enum RawJson<'a> {
    Null,
    Bool(bool),
    Num(&'a str),
    Str(&'a str),
    Arr(Vec<RawJson<'a>>),
    Obj(Vec<(RawJson<'a>, RawJson<'a>)>),
}

impl<'a> RawJson<'a> {
    pub fn eval(&self) -> Json {
        use std::str::FromStr;
        match self {
            RawJson::Null => Json::Null,
            RawJson::Bool(b) => Json::Bool(*b),
            RawJson::Num(n) => Json::Num(BigDecimal::from_str(n).unwrap()),
            RawJson::Str(s) => Json::Str(eval_str::<'_, (_, ErrorKind)>(s).unwrap().1),
            RawJson::Arr(arr) => Json::Arr(arr.iter().map(|r| r.eval()).collect()),
            RawJson::Obj(obj) => {
                let mut m = LinkedHashMap::new();
                for (key, value) in obj {
                    let key = match key.eval() {
                        Json::Str(s) => s,
                        _ => panic!("unsupported key type"),
                    };
                    m.insert(key, value.eval());
                }
                Json::Obj(m)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Json {
    Null,
    Bool(bool),
    Num(BigDecimal),
    Str(String),
    Arr(Vec<Json>),
    Obj(LinkedHashMap<String, Json>),
}

pub fn parse_and_eval_json(input: &str) -> Result<Json, String> {
    match parse_json_verbose(input) {
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => Err(nom::error::convert_error(input, e)),
        Err(nom::Err::Incomplete(_)) => Err("not enough input".to_owned()),
        Ok((_, raw)) => Ok(raw.eval())
    }
}

pub fn parse_json_verbose(input: &str) -> IResult<&str, RawJson, VerboseError<&str>> {
    parse_json::<'_, VerboseError<_>>(input)
}

pub fn parse_json_simple(input: &str) -> IResult<&str, RawJson> {
    parse_json::<'_, (_, ErrorKind)>(input)
}

pub fn parse_json<'a, E>(input: &'a str) -> IResult<&'a str, RawJson, E>
where
    E: ParseError<&'a str>,
{
    let (in0, _) = read_whitespace(input)?;
    let (in1, rst) = parse_json_internal(in0)?;
    let (in2, _) = cut(all_consuming(read_whitespace))(in1)?;
    Ok((in2, rst))
}

fn parse_null<'a, E>(input: &'a str) -> IResult<&'a str, RawJson, E>
where
    E: ParseError<&'a str>,
{
    let (input, _) = tag("null")(input)?;
    Ok((input, RawJson::Null))
}

fn parse_bool<'a, E>(input: &'a str) -> IResult<&str, RawJson, E>
where
    E: ParseError<&'a str>,
{
    alt((parse_true, parse_false))(input)
}

fn parse_num<'a, E>(input: &'a str) -> IResult<&str, RawJson, E>
where
    E: ParseError<&'a str>,
{
    let mut len = 0;
    let (i, sign) = opt(onechar('-'))(input)?;
    if sign.is_some() {
        len += 1;
    }
    let (i, d) = take_while1(|c: char| c.is_numeric())(i)?;
    len += d.len();
    let i = match opt(onechar('.'))(i)? {
        (in1, Some(_)) => {
            len += 1;
            let (in2, d) = take_while1(|c: char| c.is_numeric())(in1)?;
            len += d.len();
            in2
        }
        _ => i,
    };
    match opt(one_of("eE"))(i)? {
        (in1, Some(_)) => {
            len += 1;
            let (in1, sign) = opt(onechar('-'))(in1)?;
            if sign.is_some() {
                len += 1;
            }
            let (in1, d) = take_while1(|c: char| c.is_numeric())(in1)?;
            len += d.len();
            Ok((in1, RawJson::Num(&input[..len])))
        }
        _ => Ok((i, RawJson::Num(&input[..len]))),
    }
}

fn parse_str<'a, E>(input: &'a str) -> IResult<&'a str, RawJson, E>
where
    E: ParseError<&'a str>,
{
    let (in0, _) = onechar('"')(input)?;
    let mut i = in0;
    let start = 1;
    let mut end = 1;
    loop {
        let (in1, s) = read_non_escaped_str(i)?;
        end += s.len();
        if let Ok((in2, _)) = onechar::<_, (_, ErrorKind)>('"')(in1) {
            return Ok((in2, RawJson::Str(&input[start..end])));
        }
        let (in3, s) = read_escaped_str(in1)?;
        i = in3;
        end += s.len();
    }
}

fn eval_str<'a, E>(input: &'a str) -> IResult<&'a str, String, E>
where
    E: ParseError<&'a str>,
{
    let mut s = String::new();
    let mut i = input;
    while !i.is_empty() {
        let (in1, out1) = read_non_escaped_str(i)?;
        s.push_str(out1);
        i = in1;
        if i.is_empty() {
            break;
        }
        let (in1, c) = eval_escaped_str(i)?;
        s.push(c);
        i = in1;
    }
    Ok((i, s))
}

fn eval_escaped_str<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
where
    E: ParseError<&'a str>,
{
    let (i, _) = onechar('\\')(input)?;
    let (i, ec) = take(1usize)(i)?;
    let c = match ec {
        "\"" => '"',
        "\\" => '\\',
        "/" => '/',
        "b" => '\u{0008}',
        "f" => '\u{000C}',
        "n" => '\n',
        "r" => '\r',
        "t" => '\t',
        "u" | "U" => {
            return eval_unicode_hexstr(i);
        }
        _ => panic!("unsupported escape char"),
    };
    Ok((i, c))
}

fn eval_unicode_hexstr<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
where
    E: ParseError<&'a str>,
{
    let (input, s) = take(4usize)(input)?;
    let code = u32::from_str_radix(s, 16).unwrap();
    let c = std::char::from_u32(code).unwrap();
    Ok((input, c))
}

fn parse_arr<'a, E>(input: &'a str) -> IResult<&'a str, RawJson, E>
where
    E: ParseError<&'a str>,
{
    let (mut i, _) = onechar('[')(input)?;
    let mut arr = Vec::new();
    loop {
        // whitespace
        i = read_whitespace(i)?.0;
        // end of array
        if let Ok((in1, _)) = onechar::<_, (_, ErrorKind)>(']')(i) {
            return Ok((in1, RawJson::Arr(arr)));
        }
        // next element
        if !arr.is_empty() {
            // comma
            i = cut(onechar(','))(i)?.0;
            // whitespace
            i = read_whitespace(i)?.0;
        }
        let (in1, elem) = cut(parse_json_internal)(i)?;
        arr.push(elem);
        i = in1;
    }
}

fn parse_obj<'a, E>(input: &'a str) -> IResult<&'a str, RawJson, E>
where
    E: ParseError<&'a str>,
{
    let (mut i, _) = onechar('{')(input)?;
    let mut obj = Vec::new();
    loop {
        // whitespace
        i = read_whitespace(i)?.0;
        // end of object
        if let Ok((in1, _)) = onechar::<_, (_, ErrorKind)>('}')(i) {
            return Ok((in1, RawJson::Obj(obj)));
        }
        // next key value pair
        if !obj.is_empty() {
            // comma
            i = cut(onechar(','))(i)?.0;
            // whitespace
            i = read_whitespace(i)?.0;
        }
        let (in1, kv) = cut(parse_key_value_pair)(i)?;
        obj.push(kv);
        i = in1;
    }
}

fn parse_key_value_pair<'a, E>(input: &'a str) -> IResult<&'a str, (RawJson, RawJson), E>
where
    E: ParseError<&'a str>,
{
    let (input, key) = parse_str(input)?;
    let (input, _) = read_whitespace(input)?;
    let (input, _) = onechar(':')(input)?;
    let (input, _) = read_whitespace(input)?;
    let (input, value) = parse_json_internal(input)?;
    return Ok((input, (key, value)));
}

pub fn parse_json_internal<'a, E>(input: &'a str) -> IResult<&'a str, RawJson, E>
where
    E: ParseError<&'a str>,
{
    alt((
        parse_null, parse_bool, parse_num, parse_str, parse_arr, parse_obj,
    ))(input)
}

fn read_whitespace<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    take_while(|c| c == '\r' || c == '\n' || c == ' ' || c == '\t')(input)
}

fn parse_true<'a, E>(input: &'a str) -> IResult<&'a str, RawJson, E>
where
    E: ParseError<&'a str>,
{
    let (input, _) = tag("true")(input)?;
    Ok((input, RawJson::Bool(true)))
}

fn parse_false<'a, E>(input: &'a str) -> IResult<&'a str, RawJson, E>
where
    E: ParseError<&'a str>,
{
    let (input, _) = tag("false")(input)?;
    Ok((input, RawJson::Bool(false)))
}

fn parse_escaped_unicode<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    let (input, s) = take(4usize)(input)?;
    if s.chars().all(|c| c.is_digit(16)) {
        return Ok((input, s));
    }
    Err(nom::Err::Error(E::from_error_kind(
        input,
        ErrorKind::HexDigit,
    )))
}

fn read_non_escaped_str<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    take_while(|c: char| c != '\\' && c != '"')(input)
}

fn read_escaped_str<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    let i = input;
    let (i, _) = onechar('\\')(i)?;
    let mut len = 1;
    let (i, ec) = take(1usize)(i)?;
    len += ec.len();
    match ec {
        "\"" | "\\" | "/" | "b" | "f" | "n" | "r" | "t" => Ok((i, &input[0..len])),
        "u" | "U" => {
            let (i, s) = parse_escaped_unicode(i)?;
            len += s.len();
            Ok((i, &input[0..len]))
        }
        _ => Err(nom::Err::Error(E::from_error_kind(i, ErrorKind::Escaped))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_null() {
        assert_eq!(
            Ok(("", RawJson::Null)),
            parse_null::<'_, VerboseError<_>>("null")
        );
        assert!(parse_null::<'_, VerboseError<_>>("nul").is_err());
    }

    #[test]
    fn test_bool() {
        assert_eq!(
            Ok(("", RawJson::Bool(true))),
            parse_bool::<'_, VerboseError<_>>("true")
        );
        assert_eq!(
            Ok(("", RawJson::Bool(false))),
            parse_bool::<'_, VerboseError<_>>("false")
        );
        assert!(parse_bool::<'_, VerboseError<_>>("fal").is_err());
    }

    #[test]
    fn test_num() {
        assert_eq!(
            Ok(("", RawJson::Num("1"))),
            parse_num::<'_, VerboseError<_>>("1")
        );
        assert_eq!(
            Ok(("", RawJson::Num("-1"))),
            parse_num::<'_, VerboseError<_>>("-1")
        );
        assert_eq!(
            Ok(("", RawJson::Num("1.1"))),
            parse_num::<'_, VerboseError<_>>("1.1")
        );
        assert_eq!(
            Ok(("", RawJson::Num("1.1e1"))),
            parse_num::<'_, VerboseError<_>>("1.1e1")
        );
        assert_eq!(
            Ok(("", RawJson::Num("1.1e-1"))),
            parse_num::<'_, VerboseError<_>>("1.1e-1")
        );
    }

    #[test]
    fn test_str() {
        assert_eq!(
            Ok(("", RawJson::Str(""))),
            parse_str::<'_, VerboseError<_>>("\"\"")
        );
        assert_eq!(
            Ok(("", RawJson::Str("abc"))),
            parse_str::<'_, VerboseError<_>>("\"abc\"")
        );
        assert_eq!(
            Ok(("", RawJson::Str("\\b"))),
            parse_str::<'_, VerboseError<_>>("\"\\b\"")
        );
        assert_eq!(
            Ok(("", RawJson::Str("\\ufffe"))),
            parse_str::<'_, VerboseError<_>>("\"\\ufffe\"")
        );
        assert_eq!(
            Ok(("", RawJson::Str("he\\u0010llo"))),
            parse_str::<'_, VerboseError<_>>("\"he\\u0010llo\"")
        );
    }

    #[test]
    fn test_arr() {
        assert_eq!(
            Ok(("", RawJson::Arr(Vec::new()))),
            parse_arr::<'_, VerboseError<_>>("[]")
        );
        assert!(parse_arr::<'_, VerboseError<_>>("[").is_err());
        assert_eq!(
            Ok(("", RawJson::Arr(vec![RawJson::Null]))),
            parse_arr::<'_, VerboseError<_>>("[null]")
        );
        assert_eq!(
            Ok((
                "",
                RawJson::Arr(vec![RawJson::Bool(true), RawJson::Bool(false)])
            )),
            parse_arr::<'_, VerboseError<_>>("[true, false]")
        );
    }

    #[test]
    fn test_obj() {
        assert_eq!(
            Ok(("", RawJson::Obj(Vec::new()))),
            parse_obj::<'_, VerboseError<_>>("{}")
        );
        assert!(parse_obj::<'_, VerboseError<_>>("{").is_err());
        assert!(parse_obj::<'_, VerboseError<_>>("{1:true}").is_err());

        assert_eq!(
            Ok((
                "",
                RawJson::Obj(vec![(RawJson::Str("hello"), RawJson::Str("world"))])
            )),
            parse_obj::<'_, VerboseError<_>>(r#"{"hello":"world"}"#)
        );
    }

    #[test]
    fn test_json() {
        assert_eq!(
            Ok(("", RawJson::Null)),
            parse_json::<'_, VerboseError<_>>("null")
        );
        assert!(parse_json::<'_, VerboseError<_>>("{}123").is_err());
    }

    #[test]
    fn test_eval() {
        use std::str::FromStr;
        assert_eq!(Json::Null, parse_json_simple("null").unwrap().1.eval());
        assert_eq!(
            Json::Bool(true),
            parse_json_simple("true").unwrap().1.eval()
        );
        assert_eq!(
            Json::Str("hello\tworld".to_owned()),
            parse_json_simple("\"hello\\tworld\"").unwrap().1.eval()
        );
        assert_eq!(
            Json::Num(BigDecimal::from_str("123.45").unwrap()),
            parse_json_simple("1.2345e2").unwrap().1.eval()
        );
        assert_eq!(
            Json::Arr(vec![
                Json::Str("a".to_owned()),
                Json::Str("b".to_owned()),
                Json::Str("c".to_owned())
            ]),
            parse_json_simple(r#"["a", "b", "c"]"#).unwrap().1.eval()
        );
    }
}
