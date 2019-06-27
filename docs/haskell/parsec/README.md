## Haskell Parsec

### Introduction

I know the basics of parser combinators, mostly after reading "Functional Programming in Scala", but I've never tried my luck with Haskell's Parsec library.

### Prerequisites

The Parsec package contains a number of modules, defining the parser primitives and combinators.  The main module is `Text.Parsec`, which defines the parser type.  To use this, import the module using:

```
> import Text.Parsec
```

### Parser Primitives

The most basic parser is matching a single character.  Parsec contains parser factory functions for many things in the `Text.Parsec.Char`:

```
> import Text.Parsec.Char
```

From there, create a parser matching the character 'a', using the `char` factory function:

```
> a_char = char 'a' :: Parsec String () Char
```

The type declaration is necessary because the context doesn't contain enough information to infer the type parameters of the parser.  The type definition `Parsec String () Char` defines a parser working on strings, with a state type of `()` (the empty tuple) and returning a character.

Running this parser on a string starting with an 'a' is possible using a helper function:

```
> runParser a_char () "filename" "a"
Right 'a'
```

The `runParser` function takes a parser (a_char), a user state (the empty tuple), a filename for error reporting, and the input stream ("a").

As an example of error reporting, try running the parser on a non-matching string:

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
