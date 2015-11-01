{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Unityped.Getters where

import Control.Applicative
import Control.UnitypedParser
import Data.Unityped.Value
import Data.Unityped.Class

class HasKey a where
  (.:)  :: (FromValue r) => a -> String -> ParseM r
  infixl 6 .:

instance HasKey [AnnotatedPair] where
  (.:) pairs key = withField key parseValue pairs

instance HasKey AnnotatedValue where
  (.:) d key = withDict (withField key parseValue) d

instance (a ~ AnnotatedValue) => HasKey (ParseM a) where
  (.:) ppairs key = ppairs >>= withDict (withField key parseValue)

infixl 6 .?:
(.?:) :: (HasKey a, FromValue r) => a -> String -> ParseM (Maybe r)
(.?:) pairs key = optional (pairs .: key)

infix 5 .?=
(.?=) :: ParseM (Maybe a) -> a -> ParseM a
(.?=) p def =
  p >>= \a -> case a of
    Nothing -> return def
    Just a  -> return a

class HasColumn a where
  (.|:) :: (FromValue r) => a -> String -> ParseM [r]
  infixl 6 .|:

instance HasColumn [AnnotatedColumn] where
  (.|:) cols key = withColumn key (mapM parseValue) cols

instance HasColumn AnnotatedValue where
  (.|:) val key = withTableLike (withColumn key (mapM parseValue)) val

instance (a ~ AnnotatedValue) => HasColumn (ParseM a) where
  (.|:) pval key = pval >>= withTableLike (withColumn key (mapM parseValue))

class Indexable a where
  (.!!) :: (FromValue r) => a -> Int -> ParseM r
  infixl 6 .!!

instance Indexable AnnotatedValue where
  (.!!) arr n = withArr (withElem n parseValue) arr

instance Indexable [AnnotatedValue] where
  (.!!) els n = withElem n parseValue els

instance (a ~ [AnnotatedValue]) => Indexable (ParseM a) where
  (.!!) pval n = pval >>= withElem n parseValue

infix 3 .=
(.=) :: (ToValue a) => String -> a -> Pair
(.=) key value = key :*: toValue value

infix 3 .|
(.|) :: (ToValue a) => String -> [a] -> Column
(.|) key values = key :|: map toValue values
