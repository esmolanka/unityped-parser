{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}

module Class where

import Control.Applicative

import Control.Monad.Reader

import Parser
import Value
import RecursionSchemes

class IsValue v where
  runParserOn :: (Value -> Parser a) -> v -> Parser a

instance IsValue Value where
  runParserOn p v = p v

instance IsValue (PosAnnotated Value) where
  runParserOn p (pos, v) = local (const pos) (p v)

class FromValue a where
  parseValue :: AnnotatedValue -> Parser a

class ToValue a where
  toValue :: a -> Value

(.:) :: (FromValue a) => [AnnotatedPair] -> String -> Parser a
(.:) pairs key = withField key parseValue pairs

(.|:) :: (FromValue a) => [AnnotatedColumn] -> String -> Parser [a]
(.|:) cols key = withColumn key (mapM parseValue) cols

instance ToValue Value where
  toValue = id

instance FromValue Int where
  parseValue = withInt pure

instance ToValue Int where
  toValue = Fix . IntLit

instance FromValue Double where
  parseValue = withDouble pure

instance ToValue Double where
  toValue = Fix . DblLit

instance FromValue Bool where
  parseValue = withBool pure

instance ToValue Bool where
  toValue = Fix . BoolLit

instance FromValue String where
  parseValue = withStr pure

instance ToValue String where
  toValue = Fix . StrLit

instance (FromValue a) => FromValue [(String, [a])] where
  parseValue = withTableLike $
                 mapM $ \(k :|: vs) ->
                   (k,) <$> dive (In $ ":" ++ k)
                          (mapM (\(v,i) -> dive (In $ "[" ++ show (i :: Int) ++ "]") (parseValue v)) (zip vs [0..]))

instance (ToValue a) => ToValue [(String, [a])] where
  toValue = Fix . Table "Anonymous" . map (\(k,v) -> (k :|: map toValue v))

instance (FromValue a) => FromValue [(String, a)] where
  parseValue = withDict (mapM (\(k :*: v) -> (k,) <$> dive (In $ "." ++ k) (parseValue v)))

instance (ToValue a) => ToValue [(String, a)] where
  toValue = Fix . Dict . map (\(k,v) -> (k :*: toValue v))

