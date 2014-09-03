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

instance (a ~ [AnnotatedPair]) => HasKey (ParseM a) where
  (.:) ppairs key = ppairs >>= \pairs -> withField key parseValue pairs

infixl 6 .?:
(.?:) :: (HasKey a, FromValue r) => a -> String -> ParseM (Maybe r)
(.?:) pairs key = (Just <$> (pairs .: key)) <|> pure Nothing

infix 5 .?=
(.?=) :: ParseM (Maybe a) -> a -> ParseM a
(.?=) p def =
  p >>= \a -> case a of
    Nothing -> return def
    Just a  -> return a

infix 6 .|:
(.|:) :: (FromValue a) => [AnnotatedColumn] -> String -> ParseM [a]
(.|:) cols key = withColumn key (mapM parseValue) cols

infix 3 .=
(.=) :: (ToValue a) => String -> a -> Pair
(.=) key value = key :*: (toValue value)

infix 3 .|
(.|) :: (ToValue a) => String -> [a] -> Column
(.|) key values = key :|: map toValue values

instance ToValue Value where
  toValue = id

instance FromValue [AnnotatedPair] where
  parseValue = withDict return

instance ToValue [Pair] where
  toValue = iDict

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

