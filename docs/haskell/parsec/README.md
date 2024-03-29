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
ghci> import Text.Parsec
```

### Parser Primitives

Parsec contains parser primitives, the most basic parsers, in the
`Text.Parsec.Char` module.

```
ghci> import Text.Parsec.Char
```

The most basic parser is matching a single character.  Create a parser
matching the character 'a', using the `char` factory function:

```
ghci> a_char = char 'a' :: Parsec String () Char
```

The type declaration is necessary because the context doesn't contain enough
information to infer the type parameters of the parser.  The type definition
`Parsec String () Char` defines a parser working on strings, with a state type
of `()` (the empty tuple) and returning a character.

### Runner

Running this parser on a string starting with an 'a' is possible using a
helper function:

```
ghci> runParser a_char () "filename" "a"
Right 'a'
```

The `runParser` function takes a parser (a_char), a user state (the empty
tuple), a filename for error reporting, and the input stream ("a").

As an example of error reporting, try running the parser on a non-matching
string:

```
ghci> runParser a_char () "filename" "b"
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
ghci> runParser (string "Hello") () "" "Hello, World!"
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
ghci> runParser (oneOf "abc") () "" "d"
Left (line 1, column 1):
unexpected "d"

ghci> runParser (oneOf "abc") () "" "c"
Right 'c'
```

Finally, `noneOf` matches any character, except the ones in the given list:

```
ghci> runParser (noneOf "abc") () "" "d"
Right 'd'

ghci> runParser (noneOf "abc") () "" "c"
Left (line 1, column 1):
unexpected "c"
```

## The Parser

So, what's the parser?  The basic type turns out to be:

```haskell
newtype ParsecT s u m a
    = ParsecT {unParser :: forall b .
                   State s u
                -> (a -> State s u -> ParseError -> m b) -- consumed ok
                -> (ParseError -> m b)                   -- consumed err
                -> (a -> State s u -> ParseError -> m b) -- empty ok
                -> (ParseError -> m b)                   -- empty err
                -> m b
                }
```

Essentially, a function wrapped in a type.  The function takes five arguments:
The parser state (containing the current input stream and the user state) and
four functions to be called on specific conditions.  The result of the
function is the result of the particular called function.

The parameters to `ParsecT` are the input stream type (`s`), the user state
type (`u`), the monad type (`m`) and the result type (`a`).

As a helper, another type is defined, fixing the monad type to `Identity`:

```haskell
type Parsec s u = ParsecT s u Identity
```

Spelled out, the definition would be `type Parsec s u a = ParsecT s u Identity
a`, but it turns out currying also works for types.

### The Character

Going back to the test parser, the `char 'a'` parser resolves into:

```haskell
char c = satisfy (==c) <?> show [c]
```

This defines `char c` as a parser that satisfies `(==c)` (i.e., is equal to c)
and, in case of failure, returns the character as the error message string.

`satisfy`, in turn, is defined as:

```haskell
satisfy f = tokenPrim (\c -> show [c])
                      (\pos c _cs -> updatePosChar pos c)
                      (\c -> if f c then Just c else Nothing)
```

This defines `satisfies (==c)` by the `tokenPrim` function, which takes three
function arguments.  The first is used for "pretty-printing" the value read
from the input stream.  The second function is used to update the stream
position, in case of a successful match.  And, the last argument is the actual
processing function, which needs to return a `Maybe` value, either `Nothing`
in case of a non-match or a `Just` value in case of a match.

Digging deeper, `tokenPrim` is a helper function for creating parsers that
doesn't update a user state:

```haskell
tokenPrim showToken nextpos test
    = tokenPrimEx showToken nextpos Nothing test
```

The `Nothing` parameter to `tokenPrimEx` indicates that there's no user state
update function.  When building parsers that update a user state, we'll need
to call `tokenPrimEx` with a `Just` value that is an update function.

```haskell
tokenPrimEx showToken nextpos Nothing test
  = ParsecT $ \(State input pos user) cok _cerr _eok eerr -> do
      r <- uncons input
      case r of
        Nothing -> eerr $ unexpectError "" pos
        Just (c,cs)
         -> case test c of
              Just x -> let newpos = nextpos pos c cs
                            newstate = State cs newpos user
                        in seq newpos $ seq newstate $
                           cok x newstate (newErrorUnknown newpos)
              Nothing -> eerr $ unexpectError (showToken c) pos
```

The function starts by fetching the input stream head value by
"un-constructing" it.  If empty, the return value will be a `Nothing` and the
parser fails.  Otherwise, the `test` function is called, which returns
a `Just` value given a match, in which case the stream position is updated,
the new parser state is calculated, and the "consumed-ok" function is called.

If the `test` function returns `Nothing`, the parser calls the "empty-error"
callback function, to indicate that no part of the input stream was consumed
before the error occurred.

## Combinators

### Standard Haskell

One of the strengths—and one of the difficulties for me—of Haskell if the use
of the standard classes to do stuff.  You just have to learn a hundred classes
and four hundred functions, then you can do magic with very little effort.
Talk about a steep learning curve...

#### Functor

A parser is a `Functor`, a class defining types that can be mapped over.
This means we can change the type of the parsed value:

```
ghci> true = fmap read (string "True") :: Parsec String () Bool

ghci> runParser true () "(unknown)" "True"
Right True
```

The `Data.Functor` module contains helper functions so `fmap read (string "True")`
can be written as `read <$> string "True"` or even `string "True" <&> read`.
Personally, I like the latter, but pick one to your liking.

The other `Functor` function is `<$`, which will replace a successfully parsed
value with another.  This is useful if creating a homogenous result of
different parsed values.  For instance:

```
ghci> crlf = "\n" <$ string "\r\n" :: Parsec String () String

ghci> runParser crlf () "(unknown)" "\r\n"
Right "\n"
```

Again, the `Data.Functor` module contains an alternative for this function,
allowing us to write `string "\r\n" $> "\n"` instead.

#### Applicator

A parser is an `Applicative` instance.  This provides operations to apply
parser results to a function, combining the results.

The `pure` function embeds a value in a parser, essentially providing a
constant parser:

```
ghci> always_a = pure 'a' :: Parsec String () Char

ghci> runParser always_a () "(unknown)" "blah"
Right 'a'
```

The `<*>` function applies a parameter to a function.  This allows sequencing
parsers and applying a function to the result:

```
ghci> a_and_b = pure (,) <*> char 'a' <*> char 'b' :: Parsec String () (Char,Char)

ghci> runParser a_and_b () "(unknown)" "abcd"
Right ('a','b')
```

Using `<$>` (infix version of fmap) from `Functor` allows us to write this
without wrapping the function in a parser, which seems a little redundant:

```
ghci> a_b_and_c = (,,) <$> char 'a' <*> char 'b' <*> char 'c' :: Parsec String () (Char,Char,Char)

ghci> runParser a_b_and_c () "(unknown)" "abcd"
Right ('a','b','c')
```

A couple of other functions are available for applicatives.  The `*>` function
applies two parsers and returns the result of the second:

```
ghci> just_b = char 'a' *> char 'b' :: Parsec String () Char

ghci> runParser just_b () "(unknown)" "ab"
Right 'b'
```

Similarly, the `<*` function applies two parsers and returns the result of the
first:

```
ghci> just_a = char 'a' <* char 'b' :: Parsec String () Char

ghci> runParser just_a () "(unknown)" "ab"
Right 'a'
```

Notably, for both `*>` and `<*`, both parsers must succeed for the combined
parser to succeed.  `<*` doesn't return the result of the first parser if the
second parser fails.

#### Monad

Parsers are monads.  This gives us a few more combinators from the standard
library.  The first (`>>=`) combines two parsers by passing the result of the
first to a function providing the second:

```
ghci> if_a_then_b = char 'a' >>= \a -> char (succ a) :: Parsec String () Char

ghci> runParser if_a_then_b () "(unknown)" "ab"
Right 'b'
```

The next one (`>>`) also combines two parsers, but does so by discarding the
result of the first:

```
ghci> if_a_then_b = char 'a' >> char 'b' :: Parsec String () Char

ghci> runParser if_a_then_b () "(unknown)" "ab"
Right 'b'
```

Finally, `return` returns a parser that returns the parameter as the result,
regardless of the input:

```
ghci> always_a = return 'a'

ghci> runParser always_a () "(unknown)" ""
Right 'a'
```

If this sounds a lot like `pure`, described above, it's because the default
implementation of `return` is, in fact, `pure`.

Notice the "missing" type declaration in the expression `always_a = return 'a'`.
The type of `always_a` is `always_a :: Monad m => m Char`, which is still
flexible.  When passed to `runParser`, the monad is inferred as `Parsec String ()`
resulting in the (correct) `Parsec String () Char` typed parser. And all is
well in Magic Kingdom...

The last monad function is `fail`, which represents a failing/failed
operation.  In our case a parser that always fails:

```
ghci> unexpected = fail "Unexpected"

ghci> runParser unexpected () "(unknown)" "some input"
Left "(unknown)" (line 1, column 1):
Unexpected
```

The `Monad` type class gives access to a couple of helper functions as well.
First, `sequence` will convert a list of parsers (of the same type) to a
parser of lists (of that type):

```
ghci> string' s = sequence $ map char s :: Parsec String () String

ghci> hello = string' "Hello"

ghci> runParser hello () "(unknown)" "Hello, World!"
Right "Hello"
```

So, map the string (list of characters) to a list of character parsers and
sequence that into a parser of a list of characters.

Finally, the `Monad` class gives access to some syntactic sugar, related to
the `>>=`, `>>`, `return`, and `fail` functions; the `do` notation.

`do` allows us to sequence parsers and return a combined result (or any
result):

```
ghci> chars_ab = do { a <- char 'a'; b <- char 'b'; return (a,b) } :: Parsec String () (Char,Char)

ghci> runParser chars_ab () "(unknown)" "ab"
Right ('a','b')
```

The `do` notation may be a little more legible in script form:

```haskell
import Text.Parsec
import Text.Parsec.Char

chars_ab :: Parsec String () (Char,Char)
chars_ab = do
    a <- char 'a'
    b <- char 'b'
    return (a,b)
```

#### Alternative

The parser as an `Alternative` type class instance defines combinators for
parsing alternatives and repetition.  The `empty` constant defines a parser
that always fails.

The `<|>` infix operator combines two parsers into one that succeeds if either
of the two succeeds:

```
ghci> a_or_b = char 'a' <|> char 'b' :: Parsec String () Char

ghci> runParser a_or_b () "(unknown)" "abc"
Right 'a'

ghci> runParser a_or_b () "(unknown)" "bac"
Right 'b'

ghci> runParser a_or_b () "(unknown)" "cab"
Left "(unknown)" (line 1, column 1):
unexpected "c"
expecting "a" or "b"
```

The `some` function lifts a parser to match one or more occurrences of some
tokens.  The resulting parser returns a list of sequences:

```
ghci> import Control.Applicative (some)

ghci> as = some (char 'a') :: Parsec String () String

ghci> runParser as () "(unknown)" "aaabbbccc"
Right "aaa"

ghci> runParser as () "(unknown)" "bbbccc"
Left "(unknown)" (line 1, column 1):
unexpected "b"
expecting "a"
```

Finally, `many` matches _zero_ or more occurrences of a parser:

```
ghci> as_or_empty = many (char 'a') :: Parsec String () String

ghci> runParser as_or_empty () "(unknown)" "aaabbbccc"
Right "aaa"

ghci> runParser as_or_empty () "(unknown)" "bbbccc"
Right ""
```
