## Reversible printing/parsing

This is a library for encoding serialization format descriptions that can be used as both printers and parsers for that format, without duplicating the code.

The library is implemented in an uber-abstract way that supports printing/parsing arbitrary data structures, both as input and output. Two targets are included in the box - (1) parsing from/printing to text strings, and (2) parsing from/printing to browser URLs, with support for path segments and querystring parameters. The latter target was the primary inspiration for developing the library, and the former target is, at the moment, a toy one, more of a proof-of-concept.

The implementation is loosely based on a [2010 paper by Rendel and Ostermann](http://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf).

### Standing example
This is a working example that uses the library for printing/parsing browser URLs (aka "routes").
To see how all the pieces fit together, read on.

```purescript
data Route
    = Home
    | Profile String
    | Foo Int String

routesDef :: forall syntax. Syntax syntax => syntax PathInfo Route
routesDef =
          (Ctor::Ctor "Home") <|:|> end
    <|||> (Ctor::Ctor "Profile") <|:|> seg "profile" *|> segValue <|* end
    <|||> (Ctor::Ctor "Foo") <|:|> seg "foo" *|> query "id" <|*|> segValue <|* end

printRoute routesDef Home == "/"
printRoute routesDef (Profile "john") == "/profile/john"
printRoute routesDef (Foo 42 "bar") == "/foo/bar?id=42"

parseRoute routesDef "/" == Home
parseRoute routesDef "/profile/mary" == Profile "mary"
parseRoute routesDef "/foo/blurp?id=5" == Foo 5 "blurp"
```

### Syntax
What is called "syntax" here is either a "printer" or a "parser". We have to give them one collective name, so that we can refer to them both by it, which is necessary for expressing the encoding format without repeating it twice. In other words, we have to call both printer and parser _something_, and that word is "syntax".
In the code this is expressed by having both printer and parser implement the `Syntax` type class.

As can be seen from the signature of `routesDef` above, a thing that is a "syntax" (i.e. printer or parser) has two parameters:
* The first parameter (`PathInfo` in the example) is the "target" of the syntax - i.e. input of the parser and output of the printer.
* The second parameter (`Route` in the example) is the data type whose encoding we're describing - i.e. output of the parser and input of the printer.

Thus, a `syntax a b` can be seen as an isomorphism (of sorts) between `a` and `b`.

### Primitives
The smallest building blocks of a syntax are primitive printing/parsing operations.
In the example above three such operations are visible:
* `seg` means "there should be a path segment with exactly this value here".
* `segValue` means "this path segment matches the next route constructor parameter".
* `query` means "this route constructor parameter matches querystring parameter with the given key".
* `end` signifies end of the route.

These primitives are provided in `Data.Intertwine.Route`, but it is also possible to define your own.

### Combining primitives
Unlike regular parser combinators (think Parsec), syntax elements combine right-to-left via the `<|*|>` operator, which is right-associative. When two syntax elements of types `a` and `b` combine, they produce a syntax element of type `(a, b)` (_using Haskell tuple notation here, because it's not as cumbersome_):

```haskell
x :: syntax PathInfo a
y :: syntax PathInfo b
x <|*|> y :: syntax PathInfo (a, b)
```

This can go on to produce nested tuples:

```haskell
z :: syntax PathInfo c
z <|*|> x <|*|> y :: syntax PathInfo (c, (a, b))
```

### Injecting/applying constructors
Once a sufficiently nested tuple of values is assembled, it can be "applied" to a constructor of the ADT that we're encoding. This is done via the operator `<|:|>` and the special type `Ctor`. The result of such operation would be another syntax that describes the type of the constructor:

```purescript
data Foo
    = Bar c a b
    | Baz p q

r :: syntax PathInfo Foo
r = (Ctor::Ctor "Bar") <|:|> z <|*|> x <|*|> y
```

Here the term "apply constructor", again, means both directions: when parsing, we need to "inject" the accumulated tuple of values into the constructor to produce a value, and when printing, we need to produce such tuple from the value by "deconstructing" the constructor. This means that we can't just apply the constructor as a function, the way we would with regular parsers. Instead, we need to create a thing that can convert both directions - tuple to ADT or ADT to tuple. Sort of a "partial isomorphism".

### Iso
The thing that can convert from ADT to tuple and back again - is kind of like an isomorphism, but not exactly. The difference is that it can _fail_. When parsing, it can obviously fail when the input is unexpected, but when printing, it can _also_ fail for a less obvious reason: as noted above, such "partial isomorphism" represents only one constructor of a potentially multi-constructor ADT, and therefore, it would fail when given an ADT value constructed by a different constructor.

In the code such "partial isomorphisms" are represented by the `Iso` type, which is just a pair of functions:

```purescript
newtype Iso a b = Iso { apply :: a -> Maybe b, inverse :: b -> Maybe b }
```

If we were to encode such isomorphism by hand, it would look something like this (using the above definition of `Foo`):

```purescript
iso_bar :: Iso (c, (a, b)) Foo
iso_bar = Iso
    { apply: \(c, (a, b)) ->
        Just (Bar c a b)
    , inverse: \foo -> case foo of
        Bar c a b -> Just (c, (a, b))
        _ -> Nothing
    }
```

And then we can inject/apply this `Iso` to the tuple-typed syntax with the `<|$|>` operator:

```purescript
r :: syntax PathInfo Foo
r = iso_bar <|$|> z <|*|> x <|*|> y
```

### Autogenerating Iso
But of course, encoding such isomorphisms for every constructor isn't a lot of fun. Not to mention that it kills the whole idea of not repeating the code twice :-)

So, in order to help with that, the library provides a way to do that automatically, based on `Generic`:

```purescript
iso_bar = iso (SProxy :: SProxy "Bar")
```

Armed with this automation, we can construct our syntax description like this:

```purescript
r :: syntax PathInfo Foo
r = iso (SProxy :: SProxy "Bar") <|$|> z <|*|> x <|*|> y
```

### Slightly shorter syntax
To make the whole thing slightly more readable, we can shorten the notation `iso (SProxy :: SProxy "Bar")` slightly: we can get rid of the `iso` call by itroducing a new type `Ctor` (which is just like `SProxy`) and a new operator `<|:|>`, which would combine the effects of `iso` and `<|$|>`, producing a notation that reads more intuitively:

```purescript
r :: syntax PathInfo Foo
r = (Ctor::Ctor "Bar") <|:|> z <|*|> x <|*|> y
```

### Combining syntaxes as alernatives
The final piece of the puzzle is combining several alternative syntaxes with the operator `<|||>`. This part works pretty much the same as combining parsers in Parsec:

```purescript
u :: syntax PathInfo a
v :: syntax PathInfo a
u <|||> v :: syntax PathInfo a
```

And so, combining all of the above, we get the final result:

```purescript
x :: syntax PathInfo a
y :: syntax PathInfo b
z :: syntax PathInfo c
p :: syntax PathInfo p
q :: syntax PathInfo q

-- x <|*|> y :: syntax PathInfo (a, b)
-- z <|*|> x <|*|> y :: syntax PathInfo (c, (a, b))

data Foo
    = Bar c a b
    | Baz p q

r :: syntax PathInfo Foo
r =
          (Ctor::Ctor "Bar") <|:|> z <|*|> x <|*|> y
    <|||> (Ctor::Ctor "Baz") <|:|> p <|*|> q
```

### Actually using the syntax
Once we have the syntax definition, we can use `printRoute` and `parseRoute` to print or parse:

```purescript
r :: syntax PathInfo Foo

i = printRoute r (Bar c a b)
j = parseRoute r i
-- j == Bar c a b
```
