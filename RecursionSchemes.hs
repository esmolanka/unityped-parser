{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module RecursionSchemes where

import Control.Arrow
import Control.Monad
import Data.Traversable (Traversable)
import qualified Data.Traversable as Traversable

newtype Fix f = Fix { unFix :: f (Fix f) }

data Cofree f a = a :< (f (Cofree f a)) deriving (Functor)

class Unwrap f g where
  unwrap ::  g -> f g

instance Unwrap f (Cofree f g) where
  unwrap (_ :< f) = f

instance Unwrap f (Fix f) where
  unwrap (Fix f) = f

class (Functor w) => Comonad w where
  extract :: w a -> a
  extend :: (w a -> b) -> w a -> w b

instance (Functor f) => Comonad (Cofree f) where
  extract (a :< _) = a
  extend f w = f w :< fmap (extend f) (unwrap w)

deriving instance (Show (f (Fix f))) => Show (Fix f)
deriving instance (Eq (f (Fix f))) => Eq (Fix f)
deriving instance (Ord (f (Fix f))) => Ord (Fix f)

deriving instance (Show a, Show (f (Cofree f a))) => Show (Cofree f a)
deriving instance (Eq a, Eq (f (Cofree f a))) => Eq (Cofree f a)
deriving instance (Ord a, Ord (f (Cofree f a))) => Ord (Cofree f a)

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

cataM :: (Traversable f, Monad m) => (f a -> m a) -> Fix f -> m a
cataM alg = alg <=< Traversable.mapM (cataM alg) . unFix

para :: (Functor f) => (f (a, Fix f) -> a) -> Fix f -> a
para alg = alg . fmap (para alg &&& id) . unFix

ana :: (Functor f) => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg
