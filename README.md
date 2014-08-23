# unityped-parser

It's experimental parser from unityped data representation like JSON
to Haskell data structures. But main goal is not performance but
ability to track parsing errors, where they occur and what was
expected to get.

Example:

```haskell
-- ## Alternative + Applicative example
-- {"base": 10, "a-side": 5, "angle": 0.8}

triangle :: Value
triangle = iDict
  [ "base" :*: iDouble 10
  , "a-side" :*: iDouble 2
  , "angle" :*: iDouble 0.8
  ]

pTriangleArea :: AnnotatedValue -> Parser Double
pTriangleArea obj = pFromBaseAndHeight obj <|> pFromSidesAndAngle obj
  where
    fromBaseAndHeight :: Double -> Double -> Double
    fromBaseAndHeight b h = b * h / 2

    pFromBaseAndHeight = withDict $ \d -> fromBaseAndHeight <$> (d .: "base")
                                                            <*> (d .: "height")

    fromSidesAndAngle:: Double -> Double -> Double -> Double
    fromSidesAndAngle a b alpha = a * b * sin alpha / 2

    pFromSidesAndAngle = withDict $ \d -> fromSidesAndAngle <$> (d .: "a-side")
                                                            <*> (d .: "b-side")
                                                            <*> (d .: "alpha")

testTriangleArea :: IO ()
testTriangleArea = do
  parseIO pTriangleArea triangle

-- Outputs:
-- > Failure at @
-- >            { at @ Dict
-- >                 required .height or
-- >              at @ Dict
-- >                 { required .b-side and
-- >                   required .alpha } }
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
