
module Control.Monad.UnitypedParser.Better
  ( P.Identifier (..)
  , P.GetId (..)
  , P.Position
  , P.Context (..)
  , P.WithAnnotation (..)
  , parseError
  ) where

import Control.Parsing
import qualified Control.Monad.UnitypedParser.Parser as P
import Control.Monad.UnitypedParser.Monad ()

type UnitypedParser = Parser P.ParseM

parseError :: String -> UnitypedParser a b
parseError msg = mkParser (const $ const $ P.parseError msg)

