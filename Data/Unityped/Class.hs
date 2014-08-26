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
  runParserOn :: (Value -> Parser a) -> v -> Parser a

instance IsValue Value where
  runParserOn p v = p v

class FromValue a where
  parseValue :: AnnotatedValue -> Parser a

class ToValue a where
  toValue :: a -> Value

class Indexable a where
  (.:)  :: (FromValue r) => a -> String -> Parser r
  infixl 6 .:
  (.?:) :: (FromValue r) => a -> String -> Parser (Maybe r)
  infixl 6 .?:

instance Indexable [AnnotatedPair] where
  (.:) pairs key = withField key parseValue pairs
  (.?:) pairs key = (Just <$> withField key parseValue pairs) <|> pure Nothing

instance Indexable AnnotatedValue where
  (.:) d key = withDict (withField key parseValue) d
  (.?:) d key = (Just <$> withDict (withField key parseValue) d) <|> pure Nothing

instance (a ~ [AnnotatedPair]) => Indexable (Parser a) where
  (.:) ppairs key = ppairs >>= \pairs -> withField key parseValue pairs
  (.?:) ppairs key = ppairs >>= (\pairs -> (Just <$> withField key parseValue pairs) <|> pure Nothing)

infix 5 .?=
(.?=) :: Parser (Maybe a) -> a -> Parser a
(.?=) p def =
  p >>= \a -> case a of
    Nothing -> return def
    Just a  -> return a

infix 6 .|:
(.|:) :: (FromValue a) => [AnnotatedColumn] -> String -> Parser [a]
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

