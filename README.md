# unityped-parser

It's experimental parser from unityped data representation like JSON
to Haskell data structures. But main goal is not performance but
ability to track parsing errors, where they occur and what was
expected to get.

Example:

```haskell
-- ## Alternative + Applicative example
-- {"a-side": 5, "angle": 0.8}

triangle :: Value
triangle = iDict
  [ "a-side" .= (2.0 :: Double)
  , "angle"  .= iDouble 0.8 -- BTW, this notation is also valid
  ]

pTriangleArea :: AnnotatedValue -> Parser Double
pTriangleArea = withDict (\d -> pFromBaseAndHeight d <|> pFromSidesAndAngle d)
  where
    fromBaseAndHeight :: Double -> Double -> Double
    fromBaseAndHeight b h = b * h / 2

    pFromBaseAndHeight d = fromBaseAndHeight <$> (d .: "base")
                                             <*> (d .: "height")

    fromSidesAndAngle:: Double -> Double -> Double -> Double
    fromSidesAndAngle a b alpha = a * b * sin alpha / 2

    pFromSidesAndAngle d = fromSidesAndAngle <$> (d  .: "a-side")
                                             <*> (d  .: "b-side")
                                             <*> (d .?: "angle" .?= pi / 4)
                                                 -- default angle is 45 deg

-- ghci> parseIO pTriangleArea triangle
-- Failure:
-- @ Dict { { required .base and
--            required .height } or
--          required .b-side }
```

Please see `Test.hs` for details.

So there are simple rules:
  1. `Monad` instance defines *then* behaviour. So `a >>= b` produces
     either error message of parser `a` or message of parser `b`, depending
     on which one is failing, but not both.
  2. `Applicative` instance defines *simultaneously* behaviour. So
     something like `(,,) <$> a <*> b <*> c` will produce up to 3 error
     messages for each failing parser, combined with `and` operation.
  3. `Alternative` instance defines *choose* behaviour. So `a <|> b <|> c`
     will produce 3 error messages, combined with `or` operation.

Parser and current unityped value data type are separated and parser
does not depend on implementation of value type, so it is easy to plug
your unityped values to the parser. But the main requirement to that
data type is it should be defined in unfixed fashion and should
implement `Functor`, `Traversable`, and `Foldable` instances.

### Other nice examples

```haskell
-- ## Smart getters example

dictWithTable :: Value
dictWithTable = iDict
  [ "SomeTable" .= iTable "TBL"
    [ "X" .| [1 :: Double, 2, 3]
    , "Z" .| ["One", "Two", "Three"]
    ]
  ]

lensLike1 :: AnnotatedValue -> ParseM Int
lensLike1 v = v .: "SomeTable" .|: "X" .!! 2

lensLike2 :: AnnotatedValue -> ParseM String
lensLike2 v = v .: "SomeTable" .|: "X" .!! 2

lensLike3 :: AnnotatedValue -> ParseM String
lensLike3 v = v .: "SomeTable" .|: "S" .!! 2

together :: AnnotatedValue -> ParseM String
together v = (++) <$> lensLike3 v
                  <*> ((show <$> lensLike1 v) <|> lensLike2 v))

-- ghci> parseIO together dictWithTable
-- Failure:
-- @ Dict .SomeTable Table{TBL} { required :S and
--                                :X [2] { expected Int, got Double or
--                                         expected String, got Double } }
```
