{-# LANGUAGE RankNTypes, MultiParamTypeClasses, ConstraintKinds #-}

module Control.Parsing (
  -- * Construct and run parser
    Parser
  , mkParser
  , parse
  -- * Parser underlying typeclass
  , Parsing
  -- * Combinators
  , traversing
  , traversingSeq
  , recursive
  , (<<*)
  , (*>>)
  ) where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Applicative
import Control.Category

import Data.Traversable as T (Traversable, traverse, mapM)

newtype Codensity f a = Codensity
  { runCodensity :: forall r. (a -> f r) -> f r }

instance (Functor f) => Functor (Codensity f) where
  fmap f c = Codensity (\k -> runCodensity c (k . f))

instance (Applicative f, Monad f) => Applicative (Codensity f) where
  pure a = Codensity $ \k -> k a
  (<*>) kfa kb = Codensity $ \k -> (runCodensity kfa pure <*> runCodensity kb pure) >>= k

instance (Alternative f, Monad f) => Alternative (Codensity f) where
  empty = Codensity $ \_ -> empty
  (<|>) ka kb = Codensity $ \k -> (runCodensity ka pure <|> runCodensity kb pure) >>= k

instance Monad (Codensity f) where
  return a = Codensity $ \k -> k a
  (>>=) ka fkb = Codensity $ \k -> runCodensity ka $ \a -> runCodensity (fkb a) k

newtype Parser f a b = Parser
  { runParser :: a -> Codensity f b }

instance Functor f => Functor (Parser f a) where
  fmap f p = Parser $ \i -> fmap f (runParser p i)

instance (Applicative f, Monad f) => Applicative (Parser f a) where
  pure a = Parser $ \_ -> pure a
  (<*>) pfa pb = Parser $ \i -> runParser pfa i <*> runParser pb i

instance (Alternative f, Monad f) => Alternative (Parser f a) where
  empty = Parser $ \_ -> empty
  (<|>) pa pb = Parser $ \i -> runParser pa i <|> runParser pb i

instance Category (Parser f) where
  id = Parser $ \i -> Codensity ($ i)
  (.) pbc pab = Parser $ \i -> runParser pab i >>= runParser pbc

instance (Applicative f, Monad f) => Arrow (Parser f) where
  arr f = Parser $ \i -> Codensity $ \k -> k (f i)
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
  hole :: Parsing f => Parser f a [a]

mkParser :: (Parsing f) => (forall r. (b -> f r) -> a -> f r) -> Parser f a b
mkParser with = Parser $ \i -> Codensity $ \f -> with f i

parse :: (Parsing f) => Parser f a b -> a -> f b
parse p i = runCodensity (runParser p i) pure

traversing :: (Parsing f, Traversable t) => Parser f a b -> Parser f (t a) (t b)
traversing p = Parser $ \i -> T.traverse (runParser p) i

traversingSeq :: (Parsing f, Traversable t) => Parser f a b -> Parser f (t a) (t b)
traversingSeq p = Parser $ \i -> T.mapM (runParser p) i

recursive :: (Parsing f) => Parser f a [a] -> Parser f a b -> Parser f a [b]
recursive holes extract = arr (:[]) . extract <|> arr concat . traversingSeq (recursive holes extract) . holes

infixr 0 *>>
(*>>) :: (Parsing f, Hole f b) => Parser f a b -> Parser f b c -> Parser f a [c]
(*>>) p extract = p >>> recursive hole extract

infixr 0 <<*
(<<*) :: (Parsing f, Hole f b) => Parser f b c -> Parser f a b -> Parser f a [c]
(<<*) extract p = recursive hole extract <<< p
