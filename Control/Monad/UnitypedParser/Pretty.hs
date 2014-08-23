{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.UnitypedParser.Pretty where

import System.IO
import Text.PrettyPrint.ANSI.Leijen as PP
import Control.Monad.UnitypedParser.Parser

instance Pretty Qualifier where
  pretty (InObj i) = pretty i
  pretty (InField f) = dot <> text f
  pretty (InColumn c) = colon <> text c
  pretty (AtIndex i) = brackets (int i)

instance Pretty Identifier where
  pretty (Id s) = text s

instance Pretty FailureReason where
  pretty (FailureReason pos fls) =
    "at" <+> align (vcat [green (ppPos pos) , pretty fls])

ppPos :: Position -> Doc
ppPos = hsep . map pretty . reverse

ppBlock :: String -> [Doc] -> Doc
ppBlock op as =
  lbrace <+> align (vcat (punctuate (space <> text op) . map (align . pretty) $ as)) <+> rbrace

instance Pretty Failure where
  pretty (And as)  = ppBlock "and" (map pretty as)
  pretty (AndQ as) = ppBlock "and" (map pretty as)
  pretty (Or as)   = ppBlock "or" (map pretty as)
  pretty (OrQ as)  = ppBlock "or" (map pretty as)
  pretty (Expectation exp Nothing) = "required" <+> pretty exp
  pretty (Expectation exp (Just got)) = "expected" <+> pretty exp <> comma <+> "got" <+> pretty got
  pretty (ParseError msg) = "Parse error:" <+> text msg

ppFailureReason :: FailureReason -> Doc
ppFailureReason reason = "Failure" <+> pretty reason <> linebreak

display :: Doc -> IO ()
display = displayIO stdout . renderPretty 0.3 120

parseIO :: (Annotatible f, Show a) => (Annotated f -> Parser a) -> Raw f -> IO ()
parseIO p a =
  case parse p a of
    Left reason -> display $ ppFailureReason reason
    Right a     -> print a

