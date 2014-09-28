
module TestObj where

import Control.Applicative

import Control.Monad.UnitypedParser
import Data.Object
import Data.Object.JsonObject

structureDeep :: Object String Scalar
structureDeep = object [ "Foo" .= object [ "Bar" .= mkBool False ] ]

deepAlternatives :: AnnotatedJsonObject -> ParseM Int
deepAlternatives v = alt0 v <|> alt1 v <|>  alt2 v <|> alt3 v <|> alt4 v <|> alt5 v
  where
    alt0 v = parseJSON v
    alt1 v = v .: "Foo" .: "Bar"
    alt2 v = v .: "Foo" .: "Bar" .: "X" .!! 5
    alt3 v = v .: "Foo" .: "Baz" .!! 5
    alt4 v = v .: "Foo" .: "Baz" .!! 10
    alt5 v = v .: "Foo" .: "Bar" .: "X" .: "Elems" .!! 0

main :: IO ()
main = do
  parseIO deepAlternatives structureDeep >>= print
