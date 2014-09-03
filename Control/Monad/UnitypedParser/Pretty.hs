{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.UnitypedParser.Pretty
  ( parsePretty
  , parseIO
  )
  where

import System.IO
import Control.Arrow (left)
import Data.Functor.Foldable
import Text.PrettyPrint.ANSI.Leijen as PP

import Control.Monad.UnitypedParser.Parser

instance Pretty Qualifier where
  pretty (InObj i) = pretty i
  pretty (InField f) = dot <> text f
  pretty (InColumn c) = colon <> text c
  pretty (AtIndex i) = brackets (int i)

instance Pretty Identifier where
  pretty (Id s) = text s

ppFailureTree :: FailureTree -> Doc
ppFailureTree tree = "Failure:" <> linebreak <> cata prettyAlg tree <> linebreak
  where
    prettyAlg (Dive q doc) = yellow (pretty q) <+> doc
    prettyAlg (Expectation exp Nothing) = "required" <+> pretty exp
    prettyAlg (Expectation exp (Just got)) = "expected" <+> pretty exp <> comma <+> "got" <+> pretty got
    prettyAlg (ParseError msg) = "error:" <+> text msg
    prettyAlg (And docs) = ppBlock "and" docs
    prettyAlg (Or docs) = ppBlock "or" docs

ppBlock :: String -> [Doc] -> Doc
ppBlock op as =
  lbrace <+> align (vcat (punctuate (space <> text op) . map (align . pretty) $ as)) <+> rbrace

parsePretty :: (Annotatible f, Show a) => (Annotated f -> ParseM a) -> Raw f -> Either String a
parsePretty p a = left (flip displayS "" . renderPretty 0.3 120 . ppFailureTree) (parse p a)

parseIO :: (Annotatible f, Show a) => (Annotated f -> ParseM a) -> Raw f -> IO ()
parseIO p a =
  case parse p a of
    Left reason -> displayIO stdout . renderPretty 0.3 120 $ ppFailureTree reason
    Right a     -> print a

