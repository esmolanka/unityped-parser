{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Unityped.Class where

import Control.Applicative

import Control.Monad.UnitypedParser
import Data.Unityped.Value

class FromValue a where
  parseValue :: AnnotatedValue -> ParseM a

class ToValue a where
  toValue :: a -> Value

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

