{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Object.Combinators where

import Control.Applicative

import Control.Monad.UnitypedParser
import Data.Object.Types

class GettableByKey c k a | a -> c k where
  (.:)  :: (c r) => a -> k -> ParseM r
  infixl 6 .:

infixl 6 .?:
(.?:) :: (GettableByKey c k a, c r) => a -> k -> ParseM (Maybe r)
(.?:) pairs key = optional (pairs .: key)

infix 5 .?=
(.?=) :: ParseM (Maybe a) -> a -> ParseM a
(.?=) p def = fmap (maybe def id) p


class Indexable c a | a -> c where
  (.!!) :: (c r) => a -> Int -> ParseM r
  infixl 6 .!!

class BuildableByKey c k r | r -> c k where
  (.=) :: (c a) => k -> a -> PairF k r
  infix 3 .=
