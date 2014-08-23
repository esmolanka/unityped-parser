
module Test where

import Control.Applicative

import Class
import Parser
import Value
import Pretty

tbl1 :: Value
tbl1 = iTable "XYTable" [ "X" :|: xcol
                        , "Y" :|: ycol
                        ]
  where xcol = map toValue ( [10, 20, 30] :: [Int] )
        ycol = [iString "10", iDouble 3.14, iString "30"]

testVal1 :: Value
testVal1 = iDict [ "Foo" :*: tbl1
                 , "Fooo" :*: tbl1
                 , "Bar" :*: iString "BAR!"
                 , "Baz" :*: iArr [iInt 10, iDouble 20, iInt 30]
                 , "Buqz" :*: iDict []
                 ]

fun4 :: String -> Double -> Int -> Int -> Int
fun4 = undefined

parseer2 :: AnnotatedValue -> Parser Int
parseer2 = withDict $ \d -> do
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


testSomeComplex :: Value
testSomeComplex = toValue [ ("N", iInt 5)
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
  parseIO pFooX2orBuqzFixx testVal1
