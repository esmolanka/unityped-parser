
module Test where

import Control.Applicative

import Class
import Parser
import Value
import RecursionSchemes


tbl1 :: Value
tbl1 = Fix $ Table "XYTable" [ "X" :|: xcol
                             , "Y" :|: ycol
                             ]
  where xcol = map toValue ( [10, 20, 30] :: [Int] )
        ycol = map Fix [StrLit "10", DblLit 3.14, StrLit "30"]

testVal1 :: Value
testVal1 = Fix $ Dict [ "Foo" :*: tbl1
                      , "Fooo" :*: tbl1
                      , "Bar" :*: Fix (StrLit "BAR!")
                      , "Baz" :*: Fix (Arr $ map Fix [IntLit 10, DblLit 20, IntLit 30])
                      , "Buqz" :*: Fix (Dict [])
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

