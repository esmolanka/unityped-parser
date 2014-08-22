{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Value where

import Control.Applicative

import Data.Data
import qualified Data.Map as M

import RecursionSchemes
import Parser

data PairF e = String :*: e
  deriving (Eq, Show, Data, Typeable)

data ColumnF e = String :|: [e]
  deriving (Eq, Show, Data, Typeable)

data ValueF e =
    Dict    [PairF e]
  | Table   String [ColumnF e]
  | Arr     [e]
  | StrLit  String
  | IntLit  Int
  | DblLit  Double
  | BoolLit Bool
    deriving (Show, Eq, Data, Typeable)

type Value = Fix ValueF
type Pair = ColumnF Value
type Column = ColumnF Value

type AnnotatedValue = Cofree ValueF Position
type AnnotatedPair = PairF AnnotatedValue
type AnnotatedColumn = ColumnF AnnotatedValue

instance GetId (ValueF f)  where
  getId (Dict _)      = Id "Dict"
  getId (Table cls _) = Id $ "Table{"++cls++"}"
  getId (Arr ls)      = Id $ "Arr["++ show (length ls) ++"]"
  getId (StrLit _)    = Id "String"
  getId (IntLit _)    = Id "Int"
  getId (DblLit _)    = Id "Double"
  getId (BoolLit _)   = Id "Bool"

withDict :: ([AnnotatedPair] -> Parser a) -> AnnotatedValue -> Parser a
withDict f (pos :< (Dict pairs)) = jump pos $ dive (In "Dict") (f pairs)
withDict _ (pos :< other)        = jump pos $ expectationError (Id "Dict") other

withField :: String -> (AnnotatedValue -> Parser a) -> [AnnotatedPair] -> Parser a
withField key p pairs =
  case M.lookup key pairs' of
    Nothing -> expectationErrorField (Id $ "." ++ key)
    Just v  -> dive (In $ "." ++ key) (p v)
  where
    pairs' = M.fromList $ map (\(k :*: v) -> (k, v)) pairs

withTable :: String -> ([AnnotatedColumn] -> Parser a) -> AnnotatedValue -> Parser a
withTable cls' f (pos :< Table cls cols) = jump pos $ dive (In $ "Table{Class="++cls++"}") (check *> f cols)
  where check = if cls' == cls then pure () else expectationErrorStr (Id $ "Class=" ++ cls') (Id $ "Class="++cls)
withTable cls' _ (pos :< other) = jump pos $ expectationError (Id $ "Table{Class="++cls'++"}") other

withTableLike :: ([AnnotatedColumn] -> Parser a) -> AnnotatedValue -> Parser a
withTableLike f (pos :< Table cls cols) = jump pos $ dive (In $ "Table{Class="++cls++"}") (f cols)
withTableLike f (pos :< Dict pairs)     = jump pos $ dive (In $ "Dict") (mapM (\(k :*: v) -> (k :|:) <$> withArr return v) pairs >>= f)
withTableLike _ (pos :< other) = jump pos ( expectationError (Id $ "Table{Class=*}") other <|>
                                            expectationError (Id $ "Dict") other )

withColumn :: String -> ([AnnotatedValue] -> Parser a) -> [AnnotatedColumn] -> Parser a
withColumn key p cols =
  case M.lookup key cols' of
    Nothing -> expectationErrorField (Id $ ":" ++ key)
    Just v  -> dive (In $ ":" ++ key) (p v)
  where
    cols' = M.fromList $ map (\(k :|: v) -> (k, v)) cols

withArr :: ([AnnotatedValue] -> Parser a) -> AnnotatedValue -> Parser a
withArr f (pos :< Arr vs) = jump pos $ dive (In "Arr") (f vs)
withArr _ (pos :< other)  = jump pos $ expectationError (Id "Arr") other

withElem :: Int -> (AnnotatedValue -> Parser a) -> [AnnotatedValue] -> Parser a
withElem n p vs | n < length vs = dive (In $ "[" ++ show n ++ "]") (p (vs !! n))
                | otherwise = expectationErrorField (Id $ "[" ++ show n ++ "]")

withStr :: (String -> Parser a) -> AnnotatedValue -> Parser a
withStr f (pos :< StrLit s) = jump pos (f s)
withStr _ (pos :< other)    = jump pos $ expectationError (Id "String") other

withInt :: (Int -> Parser a) -> AnnotatedValue -> Parser a
withInt f (pos :< IntLit s) = jump pos (f s)
withInt _ (pos :< other)    = jump pos $ expectationError (Id "Int") other

withDouble :: (Double -> Parser a) -> AnnotatedValue -> Parser a
withDouble f (pos :< DblLit s) = jump pos (f s)
withDouble _ (pos :< other)    = jump pos $ expectationError (Id "DblLit") other

withBool :: (Bool -> Parser a) -> AnnotatedValue -> Parser a
withBool f (pos :< (BoolLit s)) = jump pos (f s)
withBool _ (pos :< other)       = jump pos $ expectationError (Id "BoolLit") other
