{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}

module Class where

import Control.Applicative

import Parser
import Value

class FromValue a where
  parseValue :: Value -> Parser a

class ToValue a where
  toValue :: a -> Value

(.:) :: (FromValue a) => [Pair] -> String -> Parser a
(.:) pairs key = withField key parseValue pairs

(.|:) :: (FromValue a) => [Column] -> String -> Parser [a]
(.|:) cols key = withColumn key (mapM parseValue) cols

instance FromValue Value where
  parseValue = pure

instance ToValue Value where
  toValue = id

instance FromValue Int where
  parseValue = withInt pure

instance ToValue Int where
  toValue = IntLit

instance FromValue Double where
  parseValue = withDouble pure

instance ToValue Double where
  toValue = DblLit

instance FromValue Bool where
  parseValue = withBool pure

instance ToValue Bool where
  toValue = BoolLit

instance FromValue String where
  parseValue = withStr pure

instance ToValue String where
  toValue = StrLit

instance (FromValue a) => FromValue [(String, [a])] where
  parseValue = withTableLike $
                 mapM $ \(k :|: vs) ->
                   (k,) <$> dive (In $ ":" ++ k)
                          (mapM (\(v,i) -> dive (In $ "[" ++ show (i :: Int) ++ "]") (parseValue v)) (zip vs [0..]))

instance (ToValue a) => ToValue [(String, [a])] where
  toValue = Table "Anonymous" . map (\(k,v) -> (k :|: map toValue v))

instance (FromValue a) => FromValue [(String, a)] where
  parseValue = withDict (mapM (\(k :*: v) -> (k,) <$> dive (In $ "." ++ k) (parseValue v)))

instance (ToValue a) => ToValue [(String, a)] where
  toValue = Dict . map (\(k,v) -> (k :*: toValue v))

