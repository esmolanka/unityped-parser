{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.UnitypedParser.Pretty
  ( parsePretty
  , parseIO
  )
  where

import System.IO
import Control.Arrow (left)
import Control.Comonad.Cofree (Cofree (..))
import Text.PrettyPrint.ANSI.Leijen as PP

import Control.Monad.UnitypedParser.Parser

cataAnn :: (Functor f) => (a -> f b -> b) -> Cofree f a -> b
cataAnn alg (ann :< v) = alg ann . fmap (cataAnn alg) $ v

instance Pretty Qualifier where
  pretty (InObj i) = pretty i
  pretty (InField f) = dot <> text f
  pretty (InColumn c) = colon <> text c
  pretty (AtIndex i) = brackets (int i)

instance Pretty Identifier where
  pretty (Id s) = text s

ppInContext :: [Context] -> Doc -> Doc
ppInContext [] doc = doc
ppInContext (Context s : _) doc = vcat [green . brackets . text $ s, doc]

ppFailureTree :: FailureTree -> Doc
ppFailureTree tree = "Failure:" <> linebreak <> cataAnn prettyAlg tree <> linebreak
  where
    prettyAlg _ (Dive q doc) = yellow (pretty q) <+> doc
    prettyAlg c (Expectation exp Nothing) = ppInContext c ("required" <+> pretty exp)
    prettyAlg c (Expectation exp (Just got)) = ppInContext c ("expected" <+> pretty exp <> comma <+> "got" <+> pretty got)
    prettyAlg c (ParseError msg) = ppInContext c ("error:" <+> text msg)
    prettyAlg _ (And docs) = ppBlock "and" docs
    prettyAlg _ (Or docs) = ppBlock "or" docs

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

