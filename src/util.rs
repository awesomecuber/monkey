use color_eyre::Result;
use eyre::eyre;

pub trait Parser<'a, T> {
    fn parse(&self, input: &'a [u8]) -> Result<(T, &'a [u8])>;

    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        T: 'a,
        NewOutput: 'a,
        F: Fn(T) -> NewOutput + 'a,
    {
        BoxedParser::new(move |input| {
            self.parse(input)
                .map(|(result, next_input)| (map_fn(result), next_input))
        })
    }

    fn pred<F>(self, pred_fn: F) -> BoxedParser<'a, T>
    where
        Self: Sized + 'a,
        T: 'a,
        F: Fn(&T) -> bool + 'a,
    {
        BoxedParser::new(move |input| {
            if let Ok((value, next_input)) = self.parse(input) {
                if pred_fn(&value) {
                    return Ok((value, next_input));
                }
            }
            Err(eyre!("Didn't pass predicate"))
        })
    }

    fn repeated(self) -> BoxedParser<'a, T>
    where
        Self: Sized + 'a,
        T: 'a,
    {
        BoxedParser::new(move |input| {
            let result = self.parse(input)?;
            while let Ok(result) = self.parse(input) {}
            Ok(result)
        })
    }

    fn or<P2>(self, other: P2) -> BoxedParser<'a, T>
    where
        Self: Sized + 'a,
        T: 'a,
        P2: Parser<'a, T> + 'a,
    {
        BoxedParser::new(move |input| match self.parse(input) {
            Ok(val) => Ok(val),
            Err(_) => match other.parse(input) {
                Ok(val) => Ok(val),
                Err(_) => Err(eyre!("WHOOOOOPS!!")),
            },
        })
    }
}

struct BoxedParser<'a, T> {
    parser: Box<dyn Parser<'a, T> + 'a>,
}

impl<'a, T> BoxedParser<'a, T> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, T> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, T> Parser<'a, T> for BoxedParser<'a, T> {
    fn parse(&self, input: &'a [u8]) -> Result<(T, &'a [u8])> {
        self.parser.parse(input)
    }
}

impl<'a, F, T> Parser<'a, T> for F
where
    F: Fn(&'a [u8]) -> Result<(T, &'a [u8])>,
{
    fn parse(&self, input: &'a [u8]) -> Result<(T, &'a [u8])> {
        self(input)
    }
}

pub fn text<'a>(lit: &'a [u8]) -> impl Parser<'a, &[u8]> {
    move |input: &'a [u8]| {
        if !input.starts_with(lit) {
            return Err(eyre!("Expected {lit:?}"));
        }
        Ok((&input[..lit.len()], &input[lit.len()..]))
    }
}

pub fn any_char<'a>() -> impl Parser<'a, &'a u8> {
    move |input: &'a [u8]| {
        let char = input.first().ok_or_else(|| eyre!("Unexpected EOF"))?;
        Ok((char, &input[1..]))
    }
}

pub fn char_one_of<'a>(chars: &'a [u8]) -> impl Parser<'a, &u8> {
    any_char().pred(|c| chars.contains(c))
}

pub fn ident<'a>() -> impl Parser<'a, &'a u8> {
    any_char().pred(|c| c.is_ascii_alphabetic()).repeated()
}

pub fn digit<'a>() -> impl Parser<'a, &'a u8> {
    any_char().pred(|c| c.is_ascii_digit()).repeated()
}
