
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import System.IO

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Error

import Text.PrettyPrint.ANSI.Leijen as PP

import Data.Data
import Data.Maybe

data Identifier = Id { unId :: String } deriving (Show, Eq)

data Failure =
    OrQ [FailureReason]
  | Or  [Failure]
  | AndQ [FailureReason]
  | And  [Failure]
  | Expectation Identifier (Maybe Identifier)
  | ParseError String deriving (Show)

flattenOr :: Failure -> [Failure]
flattenOr (Or fs) = concatMap flattenOr fs
flattenOr other = [other]

flattenAnd :: Failure -> [Failure]
flattenAnd (And fs) = concatMap flattenAnd fs
flattenAnd other = [other]

data Qualifier = In { unIn :: String } deriving (Show, Eq)

type Position = [ Qualifier ]

data FailureReason = FailureReason Position Failure deriving (Show)

bothReasons :: FailureReason -> FailureReason -> FailureReason
bothReasons a@(FailureReason pos af) b@(FailureReason pos' bf) =
  if | pos == pos' -> FailureReason pos (And $ concatMap flattenAnd [af, bf])
     | otherwise -> FailureReason pos (AndQ [a, b])

anyReason :: FailureReason -> FailureReason -> FailureReason
anyReason a@(FailureReason pos af) b@(FailureReason pos' bf) =
  if | pos == pos' -> FailureReason pos (Or $ concatMap flattenOr [af, bf])
     | otherwise   -> FailureReason commonPos (OrQ [a, b])
    where
      commonPos = reverse . catMaybes $ zipWith match (reverse pos) (reverse pos')
      match a b | a == b = Just a
                | otherwise = Nothing

data Result a = Success a
              | Failure FailureReason
                deriving (Functor)

instance Monad Result where
  return = Success
  (Success a) >>= fm = fm a
  Failure f >>= _ = Failure f

instance Applicative Result where
  pure = Success
  Success f <*> Success a = Success (f a)
  Success _ <*> Failure e = Failure e
  Failure e <*> Success _ = Failure e
  Failure e <*> Failure i = Failure (bothReasons e i)

instance Alternative Result where
  empty = Failure (FailureReason [] (ParseError "empty"))
  Success a <|> _         = Success a
  Failure _ <|> Success a = Success a
  Failure e <|> Failure i = Failure (anyReason e i)

instance MonadError FailureReason Result where
  throwError = Failure
  catchError (Success a) _ = return a
  catchError (Failure reason) f = f reason

newtype Parser a =
  Parser
  { unParser :: ReaderT Position Result a }
  deriving (Functor, Monad, Applicative, Alternative, MonadReader Position)

instance MonadError Failure Parser where
  throwError fs = do
    pos <- ask
    Parser $ throwError (FailureReason pos fs)
  catchError p f = case runParser p of
                     Right a -> return a
                     Left (FailureReason _ fs) -> f fs

parse :: (e -> Parser a) -> e -> Either FailureReason a
parse p a = runParser (p a)

runParser :: Parser a -> Either FailureReason a
runParser = unResult . flip runReaderT [In "@"] . unParser
  where unResult (Success a) = Right a
        unResult (Failure reason) = Left reason

expectationError :: (Data e) => Identifier -> e -> Parser a
expectationError expected got =
  throwError $ Expectation expected (Just (Id .show . toConstr $ got))

expectationErrorStr :: Identifier -> Identifier -> Parser a
expectationErrorStr expected got =
  throwError $ Expectation expected (Just got)

expectationErrorField :: Identifier -> Parser a
expectationErrorField expected =
  throwError $ Expectation expected Nothing

dive :: Qualifier -> Parser a -> Parser a
dive q = local (q :)

instance Pretty Qualifier where
  pretty (In s) = text s

instance Pretty Identifier where
  pretty (Id s) = text s

instance Pretty FailureReason where
  pretty (FailureReason pos fls) =
    "at" <+> align (vcat [green (ppPos pos) , pretty fls])
    where
      ppPos :: Position -> Doc
      ppPos = hsep . map (text . unIn) . reverse

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

parseIO :: Show a => (e -> Parser a) -> e -> IO ()
parseIO p a =
  case parse p a of
    Left reason -> display $ ppFailureReason reason
    Right a     -> print a
