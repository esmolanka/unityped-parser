{-# LANGUAGE RankNTypes, MultiParamTypeClasses, ConstraintKinds #-}

module Control.Parsing (
  -- * Construct and run parser
    Parser
  , mkParser
  , runParsing
  -- * Parser underlying typeclass
  , Parsing
  , Hole (..)
  -- * Combinators
  , traversing
  , traversing'
  , recursive
  , (<<*), (*>>)
  , (<<^), (^>>)
  , (<<<), (>>>)
  ) where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Applicative
import Control.Category

import Data.Traversable as T (Traversable, traverse, mapM)

newtype Parser f a b = Parser
  { runParser :: a -> f b }

instance Functor f => Functor (Parser f a) where
  fmap f p = Parser $ \i -> fmap f (runParser p i)

instance (Applicative f) => Applicative (Parser f a) where
  pure a = Parser $ \_ -> pure a
  (<*>) pfa pb = Parser $ \i -> runParser pfa i <*> runParser pb i

instance (Alternative f) => Alternative (Parser f a) where
  empty = Parser $ \_ -> empty
  (<|>) pa pb = Parser $ \i -> runParser pa i <|> runParser pb i

instance (Monad f) => Category (Parser f) where
  id = Parser $ \i -> return i
  (.) pbc pab = Parser $ \i -> runParser pab i >>= runParser pbc

instance (Applicative f, Monad f) => Arrow (Parser f) where
  -- Monad f constaint is needed because Category instance needs it.
  arr f = Parser $ \i -> pure (f i)
  first p = Parser $ \(i, c) -> (,) <$> runParser p i <*> pure c
  second p = Parser $ \(c, i) -> (,) <$> pure c <*> runParser p i
  (***) pa pb = Parser $ \(a, b) -> (,) <$> runParser pa a <*> runParser pb b
  (&&&) pa pb = Parser $ \i -> (,) <$> runParser pa i <*> runParser pb i

instance (Alternative f, Monad f) => ArrowZero (Parser f) where
  zeroArrow = empty

instance (Alternative f, Monad f) => ArrowPlus (Parser f) where
  (<+>) = (<|>)

type Parsing f = (Alternative f, Applicative f, Monad f)

class Hole f a where
  holes :: Parsing f => Parser f a [a]

mkParser :: (Parsing f) => (forall r. (b -> f r) -> a -> f r) -> Parser f a b
mkParser with = Parser $ \i -> with pure i

runParsing :: (Parsing f) => Parser f a b -> a -> f b
runParsing = runParser

traversing :: (Parsing f, Traversable t) => Parser f a b -> Parser f (t a) (t b)
traversing p = Parser $ \i -> T.traverse (runParser p) i

traversing' :: (Parsing f, Traversable t) => Parser f a b -> Parser f (t a) (t b)
traversing' p = Parser $ \i -> T.mapM (runParser p) i

recursive :: (Parsing f) => Parser f a [a] -> Parser f a b -> Parser f a [b]
recursive holes extract = arr (:[]) . extract <|> arr concat . traversing' (recursive holes extract) . holes

infixr 0 *>>
(*>>) :: (Parsing f, Hole f b) => Parser f a b -> Parser f b c -> Parser f a [c]
(*>>) p extract = p >>> recursive holes extract

infixr 0 <<*
(<<*) :: (Parsing f, Hole f b) => Parser f b c -> Parser f a b -> Parser f a [c]
(<<*) extract p = recursive holes extract <<< p
