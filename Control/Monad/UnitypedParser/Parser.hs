{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.UnitypedParser.Parser where

import Control.Comonad.Cofree
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except

import Data.Maybe
import Data.Functor.Foldable

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

data Qualifier = InObj Identifier
               | InField String
               | InColumn String
               | AtIndex Int
                 deriving (Show, Eq)

type Position = [ Qualifier ]

type Raw f = Fix f
type Annotated f = Cofree f Position

data FailureReason = FailureReason Position Failure deriving (Show)

getCommonNode :: Eq a => [a] -> [a] -> [a]
getCommonNode pos pos' = reverse . catMaybes $ zipWith match (reverse pos) (reverse pos')
  where match a b | a == b = Just a
                  | otherwise = Nothing

bothReasons :: (Maybe Position) -> FailureReason -> FailureReason -> FailureReason
bothReasons ctx a@(FailureReason pos af) b@(FailureReason pos' bf) =
  if | pos == pos' && Just pos == ctx
       -> FailureReason pos (And $ concatMap flattenAnd [af, bf])
     | otherwise
       -> FailureReason (fromMaybe (getCommonNode pos pos') ctx) (AndQ [a, b])

anyReason :: (Maybe Position) -> FailureReason -> FailureReason -> FailureReason
anyReason ctx a@(FailureReason pos af) b@(FailureReason pos' bf) =
  if | pos == pos' && Just pos == ctx
       -> FailureReason pos (Or $ concatMap flattenOr [af, bf])
     | otherwise -> FailureReason (fromMaybe (getCommonNode pos pos') ctx) (OrQ [a, b])

data Result a = Success a
              | Failure FailureReason
                deriving (Functor)

instance Monad Result where
  return = Success
  (Success a) >>= fm = fm a
  Failure f >>= _ = Failure f

apInContext ::  Maybe Position -> Result (a -> b) -> Result a -> Result b
apInContext pos a b = a <**> b
  where
    Success f <**> Success a = Success (f a)
    Success _ <**> Failure e = Failure e
    Failure e <**> Success _ = Failure e
    Failure e <**> Failure i = Failure (bothReasons pos e i)

instance Applicative Result where
  pure = Success
  (<*>) = apInContext Nothing

altInContext :: Maybe Position -> Result a -> Result a -> Result a
altInContext pos a b = a <||> b
  where
    Success a <||> _         = Success a
    Failure _ <||> Success a = Success a
    Failure e <||> Failure i = Failure (anyReason pos e i)

instance Alternative Result where
  empty = Failure (FailureReason [] (ParseError "empty"))
  (<|>) = altInContext Nothing

instance MonadError FailureReason Result where
  throwError = Failure
  catchError (Success a) _ = return a
  catchError (Failure reason) f = f reason

newtype Parser a =
  Parser
  { unParser :: ReaderT Position Result a }
  deriving (Functor, Monad, MonadReader Position)

instance MonadError Failure Parser where
  throwError fs = do
    pos <- ask
    Parser $ throwError (FailureReason pos fs)
  catchError p f = do
    pos <- ask
    case runParser' pos p of
      Success a -> return a
      Failure (FailureReason _ fs) -> f fs

instance Applicative Parser where
  pure = return
  fa <*> b = do
    pos <- ask
    let fa' = runReaderT (unParser fa) pos
    let b'  = runReaderT (unParser b) pos
    mkParser (\_ -> apInContext (Just pos) fa' b')

instance Alternative Parser where
  empty = throwError (ParseError "empty")
  a <|> b = do
    pos <- ask
    let a' = runReaderT (unParser a) pos
    let b' = runReaderT (unParser b) pos
    mkParser (\_ -> altInContext (Just pos) a' b')

parse :: (Annotatible f) => (Annotated f -> Parser a) -> Raw f -> Either FailureReason a
parse p = runParser . p . annotate

runParser' :: Position -> Parser a -> Result a
runParser' p = flip runReaderT p . unParser

runParser :: Parser a -> Either FailureReason a
runParser = unResult . runParser' [InObj (Id "@")]
  where unResult (Success a)      = Right a
        unResult (Failure reason) = Left reason

mkParser :: (Position -> Result a) -> Parser a
mkParser fr = Parser (ReaderT fr)

class GetId a where
  getId :: a -> Identifier
  getIn :: a -> Qualifier
  getIn = InObj . getId

expectationError :: (GetId e) => Identifier -> e -> Parser a
expectationError expected got =
  throwError $ Expectation expected (Just (getId got))

expectationErrorStr :: Identifier -> Identifier -> Parser a
expectationErrorStr expected got =
  throwError $ Expectation expected (Just got)

expectationErrorField :: Identifier -> Parser a
expectationErrorField expected =
  throwError $ Expectation expected Nothing

parseError :: String -> Parser a
parseError msg =
  throwError $ ParseError msg

dive :: Qualifier -> Parser a -> Parser a
dive q = local (q :)

jump :: Position -> Parser a -> Parser a
jump pos = local (const pos)

-- Annotations

class Annotatible f where
  annotate :: Raw f -> Annotated f
  unannotate :: Annotated f -> Raw f
