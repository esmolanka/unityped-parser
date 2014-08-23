
module Test where

import Control.Applicative
import Control.Monad

import Control.Monad.UnitypedParser
import Data.Unityped

-- ## Simple example
-- {"Hello": "World"}

-- For sake of simplicity, we won't use any clever Value construction
-- techniques, we'll construct the dictionary manually.

helloWorldDict :: Value
helloWorldDict = iDict [ ("Hello" :*: iString "World") ]

pHelloWhat :: AnnotatedValue -> Parser String
pHelloWhat obj = ("We need to say hello to "++) <$> withDict (withField "Hello" parseValue) obj

pGreetingsTo' :: AnnotatedValue -> Parser String
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
greetingsDict = iDict [ ("Greetings" :*: iArr [ iString "John"
                                              , iString "Bob"
                                              , iString "Alice"
                                              ] ) ]

-- We can return unityped value from our parser, but it'll be
-- annotated with its position in input structure, so even if you
-- defer parsing of that value, the error will contain correct
-- position of value which parser was unable to process.
pGreetingsTo :: AnnotatedValue -> Parser AnnotatedValue
pGreetingsTo = withDict (withField "Greetings" return)

pThirdPerson :: AnnotatedValue -> Parser String
pThirdPerson = pGreetingsTo >=> withArr (withElem 2 parseValue)

pFourthPerson :: AnnotatedValue -> Parser String
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
    [ "Index" :*: iInt 2
    , "Greetings" :*: iArr [ iString "John"
                           , iString "Bob"
                           , iString "Alice"
                           ]
    ]

pNthPerson :: Int -> AnnotatedValue -> Parser String
pNthPerson n = pGreetingsTo >=> withArr (withElem n parseValue)

pContextDependentGreeting :: AnnotatedValue -> Parser String
pContextDependentGreeting obj = do
  n <- withDict (.: "Index") obj
  person <- pNthPerson n obj
  return $ "Hello " ++ person ++ ". You're number " ++ show n ++ " (counting from zero ;))"

testContextDependentGreeting :: IO ()
testContextDependentGreeting = do
  parseIO pContextDependentGreeting greetingsDictWithIndex

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

tbl1 :: Value
tbl1 = iTable "XYTable" [ "X" :|: xcol
                        , "Y" :|: ycol
                        ]
  where xcol = map toValue ( [10, 20, 30] :: [Int] )
        ycol = [iString "10", iDouble 3.14, iString "30"]

val1 :: Value
val1 = iDict [ "Foo" :*: tbl1
             , "Fooo" :*: tbl1
             , "Bar" :*: iString "BAR!"
             , "Baz" :*: iArr [iInt 10, iDouble 20, iInt 30]
             , "Buqz" :*: iDict []
             ]

fun4 :: String -> Double -> Int -> Int -> Int
fun4 = undefined

parser2 :: AnnotatedValue -> Parser Int
parser2 = withDict $ \d -> do
             (a,b,c) <-
               (,,) <$> withField "Fooo" (withTable "XYTable" (withColumn "Z" (withElem 1 parseValue))) d
                    <*> withField "Baz" (withArr (withElem 1 (withDouble (return . fromIntegral . truncate)))) d
                    <*> withField "Foo" (withTable "XZTable" (withColumn "X" (withElem 1 parseValue))) d
             return $ a + b + c

pFooX2orBuqzFixx :: AnnotatedValue -> Parser Int
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
someComplexVal = toValue [ ("N", iInt 5)
                         , ("Hello", hello)
                         , ("World", world)
                         ]
  where hello = toValue [ ("Y", 10 :: Int) ]
        world = toValue [ 10 :: Int, 20, 30, 40 ]

pSomeComplex :: AnnotatedValue -> Parser Int
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
