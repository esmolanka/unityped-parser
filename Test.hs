
module Test where

import Control.Applicative
import Control.Monad

import Control.Monad.UnitypedParser
import Data.Unityped

-- ## Simple example
-- {"Hello": "World"}

helloWorldDict :: Value
helloWorldDict = iDict [ ("Hello" .= "World") ]

pHelloWhat :: AnnotatedValue -> ParseM String
pHelloWhat obj = ("We need to say hello to "++) <$> withDict (withField "Hello" parseValue) obj

pGreetingsTo' :: AnnotatedValue -> ParseM String
pGreetingsTo' obj = ("We need to say hello to "++) <$> withDict (withField "Greetings" parseValue) obj

testHelloWorld :: IO ()
testHelloWorld = do
  putStrLn "Trying \"Hello\""
  parseIO pHelloWhat helloWorldDict

  putStrLn "Trying \"Greetings\""
  parseIO pGreetingsTo' helloWorldDict

-- ## Simple example with arrays
-- {"Greetings": ["John", "Bob", "Alice"]}

greetingsDict :: Value
greetingsDict = iDict [ ("Greetings" .= ["John", "Bob", "Alice"]) ]

-- We can return unityped value from our parser, but it'll be
-- annotated with its position in input structure, so even if you
-- defer parsing of that value, the error will contain correct
-- position of value which parser was unable to process.
pGreetingsTo :: AnnotatedValue -> ParseM AnnotatedValue
pGreetingsTo = withDict (withField "Greetings" return)

pThirdPerson :: AnnotatedValue -> ParseM String
pThirdPerson = pGreetingsTo >=> withArr (withElem 2 parseValue)

pFourthPerson :: AnnotatedValue -> ParseM String
pFourthPerson = pGreetingsTo >=> withArr (withElem 3 parseValue)

testGreeting :: IO ()
testGreeting = do
  putStrLn "Third person to greet is: "
  parseIO (pThirdPerson) greetingsDict

  putStrLn "Fourth person to greet is: "
  parseIO (pFourthPerson) greetingsDict

-- ## Simple context dependent example
-- { "Index": 2
-- , "Greetings": ["John", "Bob", "Alice"] }

greetingsDictWithIndex :: Value
greetingsDictWithIndex =
  iDict
    [ "Index"     .= (2 :: Int)
    , "Greetings" .= ["John", "Bob", "Alice"]
    ]

pNthPerson :: Int -> AnnotatedValue -> ParseM String
pNthPerson n = pGreetingsTo >=> withArr (withElem n parseValue)

pContextDependentGreeting :: AnnotatedValue -> ParseM String
pContextDependentGreeting obj = do
  n <- withDict (.: "Index") obj
  person <- pNthPerson n obj
  return $ "Hello " ++ person ++ ". You're number " ++ show n ++ " (counting from zero ;))"

testContextDependentGreeting :: IO ()
testContextDependentGreeting = do
  parseIO pContextDependentGreeting greetingsDictWithIndex

-- ## Alternative + Applicative example
-- {"a-side": 5, "angle": 0.8}

triangle :: Value
triangle = iDict
  [ "a-side" .= (2.0 :: Double)
  , "angle"  .= iDouble 0.8 -- BTW, this notation is also valid
  ]

pTriangleArea :: AnnotatedValue -> ParseM Double
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

testTriangleArea :: IO ()
testTriangleArea = do
  parseIO pTriangleArea triangle

-- ## Parsing and validating triangle, using FromValue typeclass

triangle2 :: Value
triangle2 = iDict
  [ "a-side" .= (2.0 :: Double)
  , "b-side" .= (4.0 :: Double)
  , "angle"  .= (0.0 :: Double)
  ]

data Triangle = Triangle
  { aSide :: Double
  , bSide :: Double
  , alpha :: Double
  } deriving (Show)

instance FromValue Triangle where
  parseValue = withDict pFromSidesAndAngle
    where
      pFromSidesAndAngle d = do
        (a,b,alpha) <- (,,) <$> (d  .: "a-side")
                            <*> (d  .: "b-side")
                            <*> (d .?: "angle" .?= pi / 4)
        when (alpha <= 0 || alpha >= pi) $
             parseError "Angle should be in range (0, pi)"
        when (a < 0) $ parseError "Side A has negative length"
        when (b < 0) $ parseError "Side B has negative length"
        return $ Triangle a b alpha

testTriangle :: IO ()
testTriangle = do
  parseIO (parseValue ::AnnotatedValue -> ParseM Triangle) triangle2

-- ## More dictionary accessors

nestedDictionary :: Value
nestedDictionary = iDict ["Foo" .= iDict ["Bar" .= iDict ["Baz" .= iInt 10]]]

pFooBarBaz :: AnnotatedValue -> ParseM Int
pFooBarBaz d = d .: "Foo" .: "Bar" .: "Baz"

pFooBarQuux :: AnnotatedValue -> ParseM Int
pFooBarQuux d = d .: "Foo" .: "Bar" .?: "Quux" .?= (-1 :: Int)

testNestedDictionary :: IO ()
testNestedDictionary = do
  parseIO pFooBarBaz nestedDictionary
  parseIO pFooBarQuux nestedDictionary

--------------------------------------------------------------------------------
-- ## Nice stuff

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

testLensLike :: IO ()
testLensLike = parseIO (\v -> (++) <$> lensLike3 v
                                   <*> ((show <$> lensLike1 v) <|> lensLike2 v))
                       dictWithTable

--------------------------------------------------------------------------------
-- ## Messy parser examples

tbl1 :: Value
tbl1 = iTable "XYTable" [ "X" .| xcol
                        , "Y" .| ycol
                        ]
  where xcol = [10, 20, 30] :: [Int]
        ycol = [iString "10", iDouble 3.14, iString "30"]

val1 :: Value
val1 = iDict [ "Foo"  .= tbl1
             , "Fooo" .= tbl1
             , "Bar"  .= iString "BAR!"
             , "Baz"  .= [iInt 10, iDouble 20, iInt 30]
             , "Buqz" .= iDict []
             ]

fun4 :: String -> Double -> Int -> Int -> Int
fun4 = undefined

parser2 :: AnnotatedValue -> ParseM Int
parser2 = withDict $ \d -> do
             (a,b,c) <-
               (,,) <$> withField "Fooo" (withTable "XYTable" (withColumn "Z" (withElem 1 parseValue))) d
                    <*> withField "Baz" (withArr (withElem 1 (withDouble (return . fromIntegral . truncate)))) d
                    <*> withField "Foo" (withTable "XZTable" (withColumn "X" (withElem 1 parseValue))) d
             return $ a + b + c

pFooX2orBuqzFixx :: AnnotatedValue -> ParseM Int
pFooX2orBuqzFixx = withDict (\d -> fooX2 d <|> buqzFixx d)
  where fooX2 = withField "Foo" (withTable "XYTable" (\cols -> x2 cols <|> y1 cols))
        x2 = withColumn "Z" (withElem 2 parseValue)
        y1 = withColumn "Y" (withElem 1 parseValue)
        buqzFixx = withField "Buqz" $
                     withDict $ \d ->
                           d .: "Fixx"
                       <|> (fun4 <$> d .: "Fixxx" <*> d .: "QQQ" <*> d .: "r" <*> d .: "p" )
                       <|> d .: "Fixxxx"
                       <|> withField "Bazz" (\s -> read <$> parseValue s) d


someComplexVal :: Value
someComplexVal =
  iDict [ ("N"     .= iInt 5)
        , ("Hello" .= hello)
        , ("World" .= world)
        ]
  where
    -- "hello" will be transformed to Dict
    hello = [ ("Y", iInt 10)
            , ("Z", iInt 20)
            ]
    -- and "world" to Arr
    world = [ 10 :: Int, 20, 30, 40 ]

pSomeComplex :: AnnotatedValue -> ParseM Int
pSomeComplex v = do
  n  <- flip withDict v $ (.: "N")
  v2 <- flip withDict v $ withField "Hello" return
  v1 <- flip withDict v $ withField "World" return
  (+) <$> (flip withArr v1 $ withElem n parseValue) <*> (flip withDict v2 $ \d -> (d .: "Foo") <|> (read <$> d .: "Bar"))

main :: IO ()
main = do
  testHelloWorld
  testGreeting
  testContextDependentGreeting
  testTriangleArea

  parseIO pFooX2orBuqzFixx val1
  parseIO pSomeComplex someComplexVal
  parseIO parser2 someComplexVal
