{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Control.Monad.UnitypedParser.Parser where

import Control.Comonad.Cofree
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except

import Data.List
import Data.Functor.Foldable

data Identifier = Id { unId :: String } deriving (Show, Ord, Eq)

data Qualifier = InObj Identifier
               | InField String
               | InColumn String
               | AtIndex Int
                 deriving (Show, Ord, Eq)

data FailureTreeF e
  = And [e]
  | Or  [e]
  | Dive Qualifier e
  | Expectation Identifier (Maybe Identifier)
  | ParseError String
    deriving (Functor, Show, Ord, Eq)

type FailureTree = Fix FailureTreeF

type Position = [ Qualifier ]

data MergeOperation = Both | Any deriving (Eq)

mergeFailureTrees :: MergeOperation -> FailureTree -> FailureTree -> FailureTree
mergeFailureTrees mergeOp ltree rtree = go mergeOp (unFix ltree) (unFix rtree)
  where
    unFix (Fix f) = f

    go Any = goAny
    go Both = goBoth

    goBoth (Dive q1 t1) (Dive q2 t2) | q1 == q2 = Fix . Dive q1 $ goBoth (unFix t1) (unFix t2)
    goBoth (And lnodes) (andify -> rnodes)      = Fix . And $ sort $ mergeNodes lnodes rnodes
    goBoth ltree        rtree@And{}             = goBoth rtree ltree
    goBoth ltree        rtree                   = Fix $ And [ Fix ltree, Fix rtree ]

    goAny  (Dive q1 t1) (Dive q2 t2) | q1 == q2 = Fix . Dive q1 $ goAny (unFix t1) (unFix t2)
    goAny  (Or  lnodes) (orify  -> rnodes)      = Fix . Or  $ sort $ mergeNodes lnodes rnodes
    goAny  ltree        rtree@Or{}              = goAny rtree ltree
    goAny  ltree        rtree                   = Fix $ Or  [ Fix ltree, Fix rtree ]

    andify (And nodes) = nodes
    andify other = [Fix other]
    orify  (Or nodes) = nodes
    orify  other = [Fix other]

    eqQ (Fix (Dive q1 _)) (Fix (Dive q2 _)) = q1 == q2
    eqQ _ _ = False

    mergeNodes [] rs = rs
    mergeNodes ls [] = ls
    mergeNodes (l:ls) (r:rs) | l `eqQ` r = go mergeOp (unFix l) (unFix r) : mergeNodes ls rs
                             | l < r     = l : mergeNodes ls (r:rs)
                             | otherwise = r : mergeNodes (l:ls) rs

type Raw f = Fix f
type Annotated f = Cofree f Position

data Result a = Success a
              | Failure FailureTree
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
  Failure e <*> Failure i = Failure $ mergeFailureTrees Both e i

instance Alternative Result where
  empty = Failure (Fix $ ParseError "empty")
  Success a <|> _         = Success a
  Failure _ <|> Success a = Success a
  Failure e <|> Failure i = Failure $ mergeFailureTrees Any e i

newtype ParseM a = ParseM
  { unParseM :: ReaderT Position Result a }
  deriving (Functor, Monad, MonadReader Position)

instance Applicative ParseM where
  pure = return
  fa <*> b = do
    pos <- ask
    let fa' = runReaderT (unParseM fa) pos
    let b'  = runReaderT (unParseM b) pos
    mkParseM (\_ -> fa' <*> b')

instance Alternative ParseM where
  empty = throwError (Fix $ ParseError "empty")
  a <|> b = do
    pos <- ask
    let a' = runReaderT (unParseM a) pos
    let b' = runReaderT (unParseM b) pos
    mkParseM (\_ -> a' <|> b')

settle :: FailureTree -> Position -> FailureTree
settle = foldr (\q -> Fix . Dive q)

instance MonadError FailureTree ParseM where
  throwError fs = mkParseM (\pos -> Failure $ settle fs (reverse pos))
  catchError p f = do
    pos <- ask
    case runParseM' pos p of
      Success a -> return a
      Failure fs -> f fs

parse :: (Annotatible f) => (Annotated f -> ParseM a) -> Raw f -> Either FailureTree a
parse p = runParseM . p . annotate

runParseM' :: Position -> ParseM a -> Result a
runParseM' p = flip runReaderT p . unParseM

runParseM :: ParseM a -> Either FailureTree a
runParseM = unResult . runParseM' [InObj (Id "@")]
  where unResult (Success a)      = Right a
        unResult (Failure reason) = Left reason

mkParseM :: (Position -> Result a) -> ParseM a
mkParseM fr = ParseM (ReaderT fr)

class GetId a where
  getId :: a -> Identifier
  getIn :: a -> Qualifier
  getIn = InObj . getId

expectationError :: (GetId e) => Identifier -> e -> ParseM a
expectationError expected got =
  throwError $ Fix $ Expectation expected (Just (getId got))

expectationErrorStr :: Identifier -> Identifier -> ParseM a
expectationErrorStr expected got =
  throwError $ Fix $ Expectation expected (Just got)

expectationErrorField :: Identifier -> ParseM a
expectationErrorField expected =
  throwError $ Fix $ Expectation expected Nothing

parseError :: String -> ParseM a
parseError msg =
  throwError $ Fix $ ParseError msg

dive :: Qualifier -> ParseM a -> ParseM a
dive q = local (q :)

jump :: Position -> ParseM a -> ParseM a
jump pos = local (const pos)

class Annotatible f where
  annotate :: Raw f -> Annotated f
  unannotate :: Annotated f -> Raw f
