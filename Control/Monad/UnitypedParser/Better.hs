
module Control.Monad.UnitypedParser.Better
  ( UnitypedParser
  , P.Identifier (..)
  , P.GetId (..)
  , P.Position
  , P.Context (..)
  , P.WithAnnotation (..)
  , parseError
  , mkParser
  , parse
  ) where

import Control.Parsing
import qualified Control.Monad.UnitypedParser.Parser as P
import qualified Control.Monad.UnitypedParser.Pretty as P
import Control.Monad.UnitypedParser.Monad ()

type UnitypedParser = Parser P.ParseM

parseError :: String -> UnitypedParser a b
parseError msg = mkParser (const $ const $ P.parseError msg)

parse :: P.WithAnnotation a => UnitypedParser (P.Annotated a) b -> P.Raw a -> Either String b
parse p = P.parsePretty (runParsing p)
