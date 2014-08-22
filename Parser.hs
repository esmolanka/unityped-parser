{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Error

import Data.Maybe

data Identifier = Id { unId :: String } deriving (Show, Eq)

data Failure =
    OrQ [FailureReason]
  | Or  [Failure]
  | AndQ [FailureReason]
  | And  [Failure]
  | Expectation Identifier (Maybe Identifier)
  | ParseError Position String deriving (Show)

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
  empty = Failure (FailureReason [] (ParseError [] "empty"))
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

class GetId a where
  getId :: a -> Identifier

expectationError :: (GetId e) => Identifier -> e -> Parser a
expectationError expected got =
  throwError $ Expectation expected (Just (getId got))

expectationErrorStr :: Identifier -> Identifier -> Parser a
expectationErrorStr expected got =
  throwError $ Expectation expected (Just got)

expectationErrorField :: Identifier -> Parser a
expectationErrorField expected =
  throwError $ Expectation expected Nothing

dive :: Qualifier -> Parser a -> Parser a
dive q = local (q :)

jump :: Position -> Parser a -> Parser a
jump pos = local (const pos)

-- Annotations

type PosAnnotated a = (Position, a)

