
module TestBetter where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Category
import Control.Applicative

-- Unityped value construction
import Data.Unityped.Value
import Data.Unityped.Getters ((.=))

-- Better parsing API
import Data.Unityped.Better
import Control.UnitypedParser.Better
import Control.Parsing

val1 :: Value
val1 = iDict
  [ "Foo"  .= iDict [ "Bar" .= iString "BAR!!!" ]
  , "Fooo" .= iInt 20
  , "Bar"  .= iString "BAR!"
  , "Baz"  .= [ iInt 10, iDouble 20, iInt 30, iString "Wat" ]
  , "Buqz" .= iDict []
  ]

val2 :: Value
val2 = iDict
  [ "Foo"  .= iDict [ "Bar" .= iBool True ]
  , "Fooo" .= iInt 20
  , "Bar"  .= iString "BAR!"
  , "Baz"  .= [ iInt 10, iDouble 20, iInt 30 ]
  , "Buqz" .= iDict []
  ]

val3 :: Value
val3 = iDict
  [ "SomePoint" .= iDict
      [ "X" .= iDouble 0.0
      , "Y" .= iDouble 0.0 ]
  , "Triangle" .= iDict
      [ "A" .= iDict [ "X" .= iDouble 1.0
                     , "Y" .= iDouble 0.0 ]
      , "B" .= iDict [ "X" .= iDouble 3.0
                     , "Y" .= iDouble 3.0 ]
      , "C" .= iDict [ "X" .= iDouble (-1.0)
                     , "Y" .= iDouble 5.0 ]
      ]
  ]

main :: IO ()
main = do

  -- Parsing a Bool value from either Foo.Bar or Baz[2] or Baz.
  testIt $
         (key "Foo" >>> key "Bar" >>> bool)
     <|> (key "Baz" >>> index 2 >>> bool)
     <|> (key "Baz" >>> bool)

  -- Parsing a Bool value from Foo.Bar and a maximum numeric element in list Baz.
  testIt $ (,)
     <$> (key "Baz"
          >>> elems
          >>> traversing ((int >>> arr fromIntegral) <|> double)
          >>> arr maximum)
     <*> (key "Foo" >>> key "Bar" >>> bool)

  -- Recursively find and parse points encoded as dicts with fields X and Y.
  runP val3 $
         id *>> ( (,) <$> (key "X" >>> double)
                      <*> (key "Y" >>> double))


-- Helpers

runP :: (Show a) => Value -> UnitypedParser AnnotatedValue a -> IO ()
runP v p = either (putStrLn . init) print (parse p v)

testIt :: (Show a) => UnitypedParser AnnotatedValue a -> IO ()
testIt p = do
  putStrLn "On broken data:"   >> runP val1 p
  putStrLn "On correct data:"  >> runP val2 p
  putStrLn "\r\r---"
