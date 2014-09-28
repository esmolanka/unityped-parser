{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Control.Monad.UnitypedParser.Parser where

import Control.Applicative
import Control.Arrow
import Control.Comonad.Cofree
import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Set as S
import Data.Functor.Foldable

data Identifier = Id { unId :: String } deriving (Show, Ord, Eq)

data Qualifier
  = InObj Identifier
  | InField String
  | AtIndex Int
    deriving (Show, Ord, Eq)

data FailureTreeF e
  = And [e]
  | Or  [e]
  | Dive (Qualifier, Maybe Identifier) e
  | Expectation Identifier (Maybe Identifier)
  | ParseError String
    deriving (Functor, Show, Ord, Eq)

type FailureTree = Cofree FailureTreeF [Context]
type Position = [Qualifier]
newtype Context = Context String deriving (Show, Eq, Ord)
data MergeOperation = Both | Any deriving (Show, Eq)

commonPrefix :: (Eq a) => [a] -> [a] -> [a]
commonPrefix a b = reverse . map snd . takeWhile fst $ zipWith (\a b -> (a == b, a)) (reverse a) (reverse b)

mergeFailureTrees :: MergeOperation -> FailureTree -> FailureTree -> FailureTree
mergeFailureTrees mergeOp ltree rtree = go mergeOp ltree rtree
  where
    go Any  = goAny
    go Both = goBoth

    goBoth (c :< Dive q l)   (k :< Dive p r) | q == p = commonPrefix c k :< Dive q (goBoth l r)
    goBoth (c :< And lnodes) rtree@(k :< _)     = commonPrefix c k :< (mkNode And $ mergeNodes lnodes (andify rtree))
    goBoth ltree             rtree@(_ :< And{}) = goBoth rtree ltree
    goBoth ltree@(c :< _)    rtree@(k :< _)     = commonPrefix c k :< (mkNode And $ S.fromList [ltree, rtree])

    goAny  (c :< Dive q l)   (k :< Dive p r) | q == p = commonPrefix c k :< Dive q (goAny l r)
    goAny  (c :< Or  lnodes) rtree@(k :< _)     = commonPrefix c k :< (mkNode Or $ mergeNodes lnodes (orify rtree))
    goAny  ltree             rtree@(_ :< Or{})  = goAny rtree ltree
    goAny  ltree@(c :< _)    rtree@(k :< _)     = commonPrefix c k :< (mkNode Or $ S.fromList [ltree, rtree])

    andify (_ :< And nodes) = nodes
    andify other            = [other]
    orify  (_ :< Or nodes)  = nodes
    orify  other            = [other]

    mkNode node alts = case S.toList alts of
      []  -> error "Empty list of node alternatives"
      [_ :< x] -> x
      lst -> node lst

    eqQ (_ :< (Dive q1 _)) (_ :< (Dive q2 _)) = q1 == q2
    eqQ _ _ = False

    mergeNodes [] rs = S.fromList rs
    mergeNodes ls [] = S.fromList ls
    mergeNodes (l:ls) (r:rs)
      | l `eqQ` r = S.insert (go mergeOp l r) (mergeNodes ls rs)
      | l < r     = S.insert l (mergeNodes ls (r:rs))
      | otherwise = S.insert r (mergeNodes (l:ls) rs)

type Raw f = Fix f
type Annotated f = Cofree f Position

data Result a
  = Success a
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
  empty = Failure ([] :< ParseError "empty")
  Success a <|> _         = Success a
  Failure _ <|> Success a = Success a
  Failure e <|> Failure i = Failure $ mergeFailureTrees Any e i

newtype ParseM a = ParseM
  { unParseM :: ReaderT (Position, [Context]) Result a }
  deriving (Functor, Monad, MonadReader (Position, [Context]))

instance Applicative ParseM where
  pure = return
  fa <*> b = do
    env <- ask
    let fa' = runReaderT (unParseM fa) env
    let b'  = runReaderT (unParseM b) env
    mkParseM (\_ -> fa' <*> b')

instance Alternative ParseM where
  empty = throwError ([] :< ParseError "empty")
  a <|> b = do
    env <- ask
    let a' = runReaderT (unParseM a) env
    let b' = runReaderT (unParseM b) env
    mkParseM (\_ -> a' <|> b')

settle :: FailureTree -> Position -> FailureTree
settle tree = fst . foldr embed (tree, Nothing)
  where
    embed (InObj idn) (tree, _) = (tree, Just idn)
    embed q (tree, ty) = ([] :< Dive (q, ty)  tree, Nothing)

instance MonadError FailureTree ParseM where
  throwError fs = mkParseM (Failure . settle fs . reverse . fst)
  catchError p f = do
    env <- ask
    case runParseM' env p of
      Success a -> return a
      Failure fs -> f fs

parse :: (Annotatible f) => (Annotated f -> ParseM a) -> Raw f -> Either FailureTree a
parse p = runParseM . p . annotate

runParseM' :: (Position, [Context]) -> ParseM a -> Result a
runParseM' p = flip runReaderT p . unParseM

runParseM :: ParseM a -> Either FailureTree a
runParseM = unResult . runParseM' ([InObj (Id "@")], [])
  where unResult (Success a)      = Right a
        unResult (Failure reason) = Left reason

mkParseM :: ((Position, [Context]) -> Result a) -> ParseM a
mkParseM fr = ParseM (ReaderT fr)

class GetId a where
  getId :: a -> Identifier
  getIn :: a -> Qualifier
  getIn = InObj . getId

expectationError :: (GetId e) => Identifier -> e -> ParseM a
expectationError expected got = do
  ctx <- asks snd
  throwError $ (ctx :<) $ Expectation expected (Just (getId got))

expectationErrorStr :: Identifier -> Identifier -> ParseM a
expectationErrorStr expected got = do
  ctx <- asks snd
  throwError $ (ctx :<) $ Expectation expected (Just got)

expectationErrorField :: Identifier -> ParseM a
expectationErrorField expected = do
  ctx <- asks snd
  throwError $ (ctx :<) $ Expectation expected Nothing

parseError :: String -> ParseM a
parseError msg = do
  ctx <- asks snd
  throwError $ (ctx :<) $ ParseError msg

dive :: Qualifier -> ParseM a -> ParseM a
dive q = local (first (q :))

jump :: Position -> ParseM a -> ParseM a
jump pos = local (first (const pos))

label :: String -> ParseM a -> ParseM a
label s = local (second (Context s :))

label1 :: String -> (b -> ParseM a) -> b -> ParseM a
label1 s f a = label s (f a)

label2 :: String -> (c -> b -> ParseM a) -> c -> b -> ParseM a
label2 s f a b = label s (f a b)

infixr 0 <?>

(<?>) :: ParseM a -> String -> ParseM a
(<?>) p msg = label msg p

infixr 0 <?.>
(<?.>) :: (b -> ParseM a) -> String -> b -> ParseM a
(<?.>) p msg b = label msg (p b)

class Annotatible f where
  annotate :: Raw f -> Annotated f
  unannotate :: Annotated f -> Raw f
