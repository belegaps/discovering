## Haskell Parsec

### Introduction

I know the basics of parser combinators, mostly after reading "Functional
Programming in Scala", but I've never tried my luck with Haskell's Parsec
library.

### Prerequisites

The Parsec package contains a number of modules, defining the parser
primitives and combinators.  The main module is `Text.Parsec`, which defines
the parser type.  To use this, import the module using:

```
> import Text.Parsec
```

### Parser Primitives

Parsec contains parser primitives, the most basic parsers, in the
`Text.Parsec.Char` module.

```
> import Text.Parsec.Char
```

The most basic parser is matching a single character.  Create a parser
matching the character 'a', using the `char` factory function:

```
> a_char = char 'a' :: Parsec String () Char
```

The type declaration is necessary because the context doesn't contain enough
information to infer the type parameters of the parser.  The type definition
`Parsec String () Char` defines a parser working on strings, with a state type
of `()` (the empty tuple) and returning a character.

### Runner

Running this parser on a string starting with an 'a' is possible using a
helper function:

```
> runParser a_char () "filename" "a"
Right 'a'
```

The `runParser` function takes a parser (a_char), a user state (the empty
tuple), a filename for error reporting, and the input stream ("a").

As an example of error reporting, try running the parser on a non-matching
string:

```
> runParser a_char () "filename" "b"
Left "filename" (line 1, column 1):
unexpected "b"
expecting "a"
```

`runParser` returns `Either ParserError a`, where `a` is our `Char` type.  On
the successful parser run above, the return value was `Right 'a'`, indicating
a success.  In this run, the return value is an error `Left ...`, where the
contents is a parser error containing the stream location of the error
(filename, line, column), the error (unexpected "b"), and the expectation
(expecting "a").

### Additional Primitives

The `Text.Parsec.Char` module contains other simple primitives (like `tab` and
`newline`), parsers matching character classes (like `digit`, `hexDigit`,
`upper`, `lower`), and combining parsers (`endOfLine`, matching either a
newline character or a carriage return followed by a newline, and `spaces`
matching _zero_ or more whitespace characters).

Finally, there are factory functions for creating parsers.  For instance, the
`string` function, creating a parser matching the contents of a string:

```
> runParser (string "Hello") () "" "Hello, World!"
Right "Hello"
```

Note that the parser doesn't try matching the entire input stream.  Only the
part it is designed to recognize.  Technically, the parser returns the
unparsed stream, but `runParser` discards it and returns only the parsed
content.

Also, note that in this case, we didn't need the type declaration for the
parser, since both the user state (the empty tuple) and stream type (a
string) are passed to `runParser`.

The `oneOf` function creates a parser matching any one of the characters in a
given list:

```
> runParser (oneOf "abc") () "" "d"
Left (line 1, column 1):
unexpected "d"

> runParser (oneOf "abc") () "" "c"
Right 'c'
```

Finally, `noneOf` matches any character, except the ones in the given list:

```
> runParser (noneOf "abc") () "" "d"
Right 'd'

> runParser (noneOf "abc") () "" "c"
Left (line 1, column 1):
unexpected "c"
```
