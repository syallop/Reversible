# Reversible - experimental

This package defines some abstractions for working with pairs of functions which
are reversible and partial.

`Reversible.Iso` defines reversible functions. `Reversible` builds upon this and
defines reversible instructions compatible with 
[DSL-Compose](https://github.com/syallop/DSL-Compose) - an Operational-like DSL
composition library.

## Reversible.Iso
`Reversible.Iso` defines `Iso a b` as an object which can translate between two
types `a` and `b` with the possibility for failure in each direction.
Round-trips should not fail. This is sometimes called a Partial Isomorphism.

### Partial
A partial function is one that may fail. Parsing Text into some Expression
is one such example.

```haskell
parseExpr :: Text -> Maybe Expression
```

### Reversible and partial
In some cases, there may be another partial function that goes in the
'opposite' direction. Printing an Expression to Text is a corresponding
example.

```haskell
printExpr :: Expression -> Maybe String
```

If a pair of partial functions are also 'reversible' then you should be able to
roundtrip through them without changing the result. This means that:

```haskell
parse . print . parse === parse
print . parse . print === print
```

This is <b>not</b> the same property as being able to compose one function
with its inverse producing no change. For example you could imagine a
parse function which accepts duplicate whitespace whereas the print
function emits only one.

```haskell
parse . print =/= id
print . parse =/= id
```

This pattern appears in a number of places, but particularly when parsing and
unparsing between two formats of the same data.

| parse | unparse |
| ---   | ---     |
| `parse     :: Text -> Maybe Expression` | `print       :: Expression -> Maybe Text` |
| `toJSON    :: JSON -> Maybe Text`       | `fromJSON    :: Text -> Maybe a`          |
| `serialize :: a -> Maybe ByteString`    | `unserialize :: ByteString -> Maybe a`    |

### Abstracting
This pattern is sometimes called a `partial isomorphism`. We'll call it an `Iso`

```haskell
data Iso a b = Iso
  { forwards  :: a -> Maybe b
  , backwards :: b -> Maybe a
  }
```

Our reversible functions can now be paired like so:

```haskell
bytesTextIso :: Iso BytesString Text
textExprIso  :: Iso Text Expression
exprJsonIso  :: Iso Expression JSON
jsonTextIso  :: Iso JSON Text
textBytesIso :: Iso Text ByteString
```

### Chaining

We could chain these Isos together to:

- Read a Bytestrings Textual representation
- Parse Text to Expressions
- Convert the Expression to JSON
- Convert the JSON to its textual representation
- Convert the Text back to Bytes to be transmitted over the network

Taking advantage of `Maybe`'s `Monad` instance, we could use `do` notation and write:

```haskell
do txt     <- parse bytesTextIso bsIn
   exp     <- parse textExprIso txt
   json    <- parse exprJsonIso exp
   jsonTxt <- parse jsonTextIso json
   bsOut   <- parse textBytesIso jsonText
   return bsOut
```

Or use `bind` directly:

```
parse bytesTextIso bsIn >>= parse textExprIso >>= parse exprJsonIso >>= parse jsonTextIso >>= parse textBytesIso
```

### Reversing
If we arrange the `Iso`'s such that the type with the 'least' states comes first,
then many operations will tend to chain only `parses`
or only `prints` depending on whether we're parsing or printing as a whole.

Deciding on a definition of 'least' is sometimes non obvious or not particularly meaningful.
`textBytes` and `bytesText` are both the 'same' Iso in two different directions.

It might be nice to be able to reverse an Iso..

```haskell
reverse :: Iso a b -> Iso b a
reverse (Iso f g) = Iso g f

textBytesIso :: Iso Text BytesString
textBytesIso = reverse bytesTextIso
```
.. done!

Now we have a chain of parses like:

```haskell
parse bytesTextIso bsIn >>= parse textExprIso >>= parse exprJsonIso >>= parse jsonTextIso >>= parse (reverse bytesTextIso)
```

### Composing
It would be nicer still if we could compose `Iso`s together and execute `parse`
or `print` a single time. We can do that too:

```haskell
composeIso :: Iso a b -> Iso b c -> Iso a c
```

Fortunately this forms a `Category` and so we can make use of Haskells existing
composition function `.` rather than defining our own:

```haskell
import Control.Category
instance Category Iso where
  ...
```

Our chain now looks like:

```haskell
parse (bytesTextIso . textExprIso . exprJsonIso . jsonTextIso . reverse bytesTextIso) bsIn
```

## Reversible
The top-level `Reversible` module defines `ReversibleInstr`. This is a
[DSL-Compose](https://github/com/syallop/DSL-Compose) style Instruction built
upon `Reversible.Iso`. It is intended to represent 'Program's which may use
Functor, Applicative and Alternative-like functions which can be reversed. This
restriction results in incompatible type signatures to the standard
type-classes.

Below is an overview of _what_ is exported, mostly ignoring the _why_ or _how_.

### Reversible Functor
A regular Functors map has a signature like `map :: (a -> b) -> f a -> f b`.
To make this reversible, we replace the function `a -> b` with an `Iso a b`
from `Reversible.Iso` to produce `rmap :: Iso a b -> Reversible i a -> Reversible i b`.
`rmap` is also named `\$/`.

### Reversible Alternative
Alternative `empty` has a signature like `empty :: f a`. The reversible
equivalent is `rempty :: Reversible i a`. `rempty` encodes the notion of
failure. 

Alternative `<|>` has a signature like `<|> :: f a -> f a -> f a` and encodes a
notion of choice. `\|/ :: Reversible i a -> Reversible i a -> Reversible i a`. 

### Reversible Applicative
Applicative `pure` has a signature like `pure :: a -> f a`. To inject a value
into a Reversible context, we add an equality constraint to `a` to produce
`rpure :: Eq a => a -> Reversible i a`.

Applicative `<*>` has a signature like `<*> :: f (a -> b) -> f a -> f b`. The
equivalent tuples its results to permit reversal like
`\*/ :: Reversible i a -> Reversible i b -> Reversible i (a,b)`. This is typed
this way to allow writing functions like `isoF \$/ reversibleA \*/ reversibleB`. 

The equivalent to `*>` and `<*` are `*/` and `\*` their type signatures differ
in that the value on the 'discarded' side must be `()`. I.E. `*/ :: Reversible i
() -> Reversible i a -> Reversible i a` NOT `*/ :: Reversible i b -> Reversible
i a -> Reversible i a`.

### Ignoring
Combinators `allowed, required, prefered` are provided with signature
`:: Reversible i () -> Reversible i ()` and allow Reversibles to be ignored when
applied in either direction. 

`allowed` means a reversible computation is allowed to succeed when ran forwards but
ignored backwards.
 
`required` means a reversible computation is required to succeed when ran forwards and runs
backwards.

`prefered` means a reversible computation is allowed to succeed when ran forwards, and runs
backwards.

