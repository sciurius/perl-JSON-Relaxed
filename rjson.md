# Relaxed JSON (RJSON)

See [RelaxedJSON.org](https://relaxedjson.org) for full details.

## JSON

All standard JSON is valid RJSON.

## Reserved characters

The following characters are _reserved_ (have a special meaning):

    [ ] { } , : " ' `
	
Whitespace characters are also reserved.

## Reserved words

The following word sequences must be quoted when used as a string
value:

    true
	false
	null

Anything that looks like a number must be quoted when used as a string
value.

Reserved words will always be considered strings when used as an
object key.

## Comments

RJSON supports JavaScript-like comments:

    /* inline comments */
    // line-based comments

## Trailing commas

RJSON allow treats commas as separators. If nothing is after, or
between commas, those commas are just ignored:

    [
        "data",
        , // nothing after this comma
    ]

Note that the specification disallows loose commas at the beginning of
a list.

## Quotes are optional around simple keys and simple values

If a key does not contain an unescaped reserved character, it is a
_simple-key_ and does not require quotes. Reserved words (numbers and
the values true, false, and null are always interpreted as strings
when used as an object key.

If a value does not contain an unescaped reserved character,
and is not a reserved word, it is a _simple-value_
and does not require quotes.

## Single quotes, double quotes, backticks, no quotes

Strings can be quoted with single quotes, double quotes, and
backticks. 

## Escaped characters

Strings may contain escaped characters like `\t` for tab and
`\n` for newline. Reserved characters may be escaped by preceding the
character with a backslash `\`.

A string without unescaped reserved characters can omit the quotes.
So, the following data items are equivalent:

    "Star:flower"
    'Star:flower'
	`Star:flower`
    Star\:flower

A quoted string must escape its quote when it is part of the
string:

    "a\"b'c`d"
    'a"b\'c`d'
    `a"b'c\`d`

Arbitrary unicode characters can be escaped with `\u` followed by
exactly four hexadecimal digits.

## Documents that are just a single string

Early versions of JSON require that a JSON document contains either a single
hash or a single array.  Later versions also allow a single string.  RJSON
follows that later rule, so the following is a valid RJSON document:

    "Hello world"

## Commas are optional between objects pairs and array items

    {
      buy: [ milk eggs butter 'dog bones' ]
      tasks: [ { name:exercise completed:false }
               { name:eat completed:true } ]
    }

# JSON::Relaxed extensions

## Hash keys without values

`JSON::Relaxed` supports object keys without a specified value. In
that case the corresponding hash element is simply assigned the
undefined value.

In the following example, `a` is assigned `1`, and `b` is assigned _undef_:

    { a:1, b }

## Unicode escapes

`JSON::Relaxed` supports unicode escapes with an arbitrary number of
hexadecimal digits enclosed in braces:

    \u{1d10e}

Being able to use more than 4 hexadecimal digits eliminates the need
to use [Unicode surrogates](https://unicode.org/faq/utf_bom.html#utf16-2) like

    \ud834\udd0e

## String continuation

`JSON::Relaxed` allows quoted strings to be split over multiple lines:

    "Star" \
    "flower"

This produces a single string, `"Starflower"`.
Note that this is different from

	"Star \
	flower"

which produces `"Star \nflower"` (where `\n` is a newline).

