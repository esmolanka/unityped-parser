{-# LANGUAGE DeriveDataTypeable #-}

module Value where

import Control.Applicative
import Control.Monad

import Data.Data
import qualified Data.Map as M

import Parser

data Pair = String :*: Value
  deriving (Eq, Show, Data, Typeable)

data Column = String :|: [Value]
  deriving (Eq, Show, Data, Typeable)

data Value =
    Dict    [Pair]
  | Table   String [Column]
  | Arr     [Value]
  | StrLit  String
  | IntLit  Int
  | DblLit  Double
  | BoolLit Bool
    deriving (Show, Eq, Data, Typeable)

withDict :: ([Pair] -> Parser a) -> Value -> Parser a
withDict f (Dict pairs) = dive (In "Dict") (f pairs)
withDict _ other        = expectationError (Id "Dict") other

withField :: String -> (Value -> Parser a) -> [Pair] -> Parser a
withField key p pairs =
  case M.lookup key pairs' of
    Nothing -> expectationErrorField (Id $ "." ++ key)
    Just v  -> dive (In $ "." ++ key) (p v)
  where
    pairs' = M.fromList $ map (\(k :*: v) -> (k, v)) pairs

withTable :: String -> ([Column] -> Parser a) -> Value -> Parser a
withTable cls' f (Table cls cols) = dive (In $ "Table{Class="++cls++"}") (check *> f cols)
  where check = if cls' == cls then pure () else expectationErrorStr (Id $ "Class=" ++ cls') (Id $ "Class="++cls)
withTable cls' _ other = expectationError (Id $ "Table{Class="++cls'++"}") other

withTableLike :: ([Column] -> Parser a) -> Value -> Parser a
withTableLike f (Table cls cols) = dive (In $ "Table{Class="++cls++"}") (f cols)
withTableLike f (Dict pairs) = dive (In $ "Dict") (mapM (\(k :*: v) -> (k :|:) <$> withArr return v) pairs >>= f)
withTableLike _ other = expectationError (Id $ "Table{Class=*}") other <|>
                        expectationError (Id $ "Dict") other

withColumn :: String -> ([Value] -> Parser a) -> [Column] -> Parser a
withColumn key p cols =
  case M.lookup key cols' of
    Nothing -> expectationErrorField (Id $ ":" ++ key)
    Just v  -> dive (In $ ":" ++ key) (p v)
  where
    cols' = M.fromList $ map (\(k :|: v) -> (k, v)) cols

withArr :: ([Value] -> Parser a) -> Value -> Parser a
withArr f (Arr vs) = dive (In "Arr") (f vs)
withArr _ other = expectationError (Id "Arr") other

withElem :: Int -> (Value -> Parser a) -> [Value] -> Parser a
withElem n p vs | n < length vs = dive (In $ "[" ++ show n ++ "]") (p (vs !! n))
                | otherwise = expectationErrorField (Id $ "[" ++ show n ++ "]")

withStr :: (String -> Parser a) -> Value -> Parser a
withStr f (StrLit s) = f s
withStr _ other = expectationError (Id "StrLit") other

withInt :: (Int -> Parser a) -> Value -> Parser a
withInt f (IntLit s) = f s
withInt _ other = expectationError (Id "IntLit") other

withDouble :: (Double -> Parser a) -> Value -> Parser a
withDouble f (DblLit s) = f s
withDouble _ other = expectationError (Id "DblLit") other

withBool :: (Bool -> Parser a) -> Value -> Parser a
withBool f (BoolLit s) = f s
withBool _ other = expectationError (Id "BoolLit") other
