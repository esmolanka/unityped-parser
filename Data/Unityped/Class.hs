{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Unityped.Class where

import Control.Applicative

import Control.Monad.UnitypedParser
import Data.Unityped.Value

class IsValue v where
  runParseMOn :: (Value -> ParseM a) -> v -> ParseM a

instance IsValue Value where
  runParseMOn p v = p v

class FromValue a where
  parseValue :: AnnotatedValue -> ParseM a

class ToValue a where
  toValue :: a -> Value

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
(.?:) pairs key = (Just <$> (pairs .: key)) <|> pure Nothing

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
  (.!!) arr n = withArr (\els -> withElem n parseValue els) arr

instance Indexable [AnnotatedValue] where
  (.!!) els n = withElem n parseValue els

instance (a ~ [AnnotatedValue]) => Indexable (ParseM a) where
  (.!!) pval n = pval >>= withElem n parseValue

infix 3 .=
(.=) :: (ToValue a) => String -> a -> Pair
(.=) key value = key :*: (toValue value)

infix 3 .|
(.|) :: (ToValue a) => String -> [a] -> Column
(.|) key values = key :|: map toValue values

instance ToValue Value where
  toValue = id

instance FromValue AnnotatedValue where
  parseValue = return

instance FromValue Int where
  parseValue = withInt pure

instance ToValue Int where
  toValue = iInt

instance FromValue Double where
  parseValue = withDouble pure

instance ToValue Double where
  toValue = iDouble

instance FromValue Bool where
  parseValue = withBool pure

instance ToValue Bool where
  toValue = iBool

instance FromValue String where
  parseValue = withStr pure

instance ToValue String where
  toValue = iString

instance (FromValue a) => FromValue [(String, [a])] where
  parseValue = withTableLike $ withColumns $ withElems parseValue

instance (ToValue a) => ToValue [(String, [a])] where
  toValue = iTable "Anonymous" . map (\(k,v) -> (k :|: map toValue v))

instance (FromValue a) => FromValue [(String, a)] where
  parseValue = withDict $ withFields parseValue

instance (ToValue a) => ToValue [(String, a)] where
  toValue = iDict . map (\(k,v) -> (k :*: toValue v))

instance (FromValue a) => FromValue [a] where
  parseValue = withArr $ withElems parseValue

instance (ToValue a) => ToValue [a] where
  toValue = iArr . map toValue

