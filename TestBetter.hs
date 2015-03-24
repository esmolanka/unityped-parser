
import Prelude hiding ((.), id)

import Control.Applicative
import Control.Arrow
import Control.Category

import Data.Unityped.Value
import Data.Unityped.Getters ((.=))
import Data.Unityped.Better
import Control.Monad.UnitypedParser.Better

import Control.Parsing

val1 :: Value
val1 = iDict [ "Foo"  .= iDict [ "Bar" .= iString "BAR!!!" ]
             , "Fooo" .= iInt 20
             , "Bar"  .= iString "BAR!"
             , "Baz"  .= [iInt 10, iDouble 20, iInt 30]
             , "Buqz" .= iDict []
             ]

main :: IO ()
main = do
  either putStrLn print $ parse (key "Foo" >>> key "Bar" >>> string) val1
  either putStrLn print $ parse (id *>> int) val1
