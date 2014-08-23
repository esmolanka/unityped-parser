{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module Value
  ( iDict
  , iTable
  , iArr
  , iString
  , iInt
  , iDouble
  , iBool
  , PairF (..)
  , ColumnF (..)
  , ValueF
  , Value
  , AnnotatedValue
  , Pair
  , AnnotatedPair
  , Column
  , AnnotatedColumn
  , withPos
  , withDict
  , withField
  , withFields
  , withTable
  , withTableLike
  , withColumn
  , withColumns
  , withArr
  , withElem
  , withElems
  , withStr
  , withInt
  , withDouble
  , withBool
  ) where

import Control.Applicative
import Control.Monad.Reader

import Data.Data
import qualified Data.Map as M

import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
import qualified Data.Traversable as Tr

import RecursionSchemes
import Parser

data PairF e = String :*: e
  deriving (Eq, Show, Data, Typeable, Functor, Traversable, Foldable)

data ColumnF e = String :|: [e]
  deriving (Eq, Show, Data, Typeable, Functor, Traversable, Foldable)

data ValueF e =
    Dict    [PairF e]
  | Table   String [ColumnF e]
  | Arr     [e]
  | StrLit  String
  | IntLit  Int
  | DblLit  Double
  | BoolLit Bool
    deriving (Show, Eq, Data, Typeable, Functor, Traversable, Foldable)

type Value  = Fix ValueF
type Pair   = PairF Value
type Column = ColumnF Value

type AnnotatedValue  = Annotated ValueF
type AnnotatedPair   = PairF AnnotatedValue
type AnnotatedColumn = ColumnF AnnotatedValue

iDict :: [Pair] -> Value
iDict = Fix . Dict

iTable :: String -> [Column] -> Value
iTable cls = Fix . Table cls

iArr :: [Value] -> Value
iArr = Fix . Arr

iString :: String -> Fix ValueF
iString = Fix . StrLit

iInt :: Int -> Fix ValueF
iInt = Fix . IntLit

iDouble :: Double -> Fix ValueF
iDouble = Fix . DblLit

iBool :: Bool -> Fix ValueF
iBool = Fix . BoolLit

instance Annotatible ValueF where
  unannotate = undefined
  annotate root = runReader (cata alg root) [InObj (Id "@")]
    where
      alg :: ValueF (Reader Position (Cofree ValueF Position)) -> Reader Position (Cofree ValueF Position)
      alg obj@(Dict pairs) = (:<) <$> ask <*> local (getIn obj:) (Dict <$> mapM annotateField pairs)
        where
          annotateField :: PairF (Reader Position (Cofree ValueF Position)) -> Reader Position AnnotatedPair
          annotateField (k :*: rv) = fmap (k :*:) (local (InField k:) rv)
      alg other = (:<) <$> ask <*> (Tr.sequence other)

instance GetId (ValueF f)  where
  getId (Dict _)      = Id "Dict"
  getId (Table cls _) = Id $ "Table{"++cls++"}"
  getId (Arr ls)      = Id $ "Arr["++ show (length ls) ++"]"
  getId (StrLit _)    = Id "String"
  getId (IntLit _)    = Id "Int"
  getId (DblLit _)    = Id "Double"
  getId (BoolLit _)   = Id "Bool"

withPos :: AnnotatedValue -> (ValueF (AnnotatedValue) -> Parser a) -> Parser a
withPos (pos :< obj) f = jump pos (f obj)

withDict :: ([AnnotatedPair] -> Parser a) -> AnnotatedValue -> Parser a
withDict f (pos :< o@(Dict pairs)) = jump pos $ dive (getIn o) (f pairs)
withDict _ (pos :< other)          = jump pos $ expectationError (Id "Dict") other

withField :: String -> (AnnotatedValue -> Parser a) -> [AnnotatedPair] -> Parser a
withField key p pairs =
  case M.lookup key pairs' of
    Nothing -> expectationErrorField (Id $ "." ++ key)
    Just v  -> dive (InField key) (p v)
  where
    pairs' = M.fromList $ map (\(k :*: v) -> (k, v)) pairs

withFields :: (AnnotatedValue -> Parser a) -> [AnnotatedPair] -> Parser [(String, a)]
withFields p = mapM (\(k :*: v) -> (k,) <$> dive (InField k) (p v))

withTable :: String -> ([AnnotatedColumn] -> Parser a) -> AnnotatedValue -> Parser a
withTable cls' f (pos :< o@(Table cls cols)) = jump pos $ dive (getIn o) (check *> f cols)
  where check = if cls' == cls then pure () else expectationErrorStr (Id $ "Class=" ++ cls') (Id $ "Class="++cls)
withTable cls' _ (pos :< other) = jump pos $ expectationError (Id $ "Table{Class="++cls'++"}") other

withTableLike :: ([AnnotatedColumn] -> Parser a) -> AnnotatedValue -> Parser a
withTableLike f (pos :< o@(Table _ cols)) = jump pos $ dive (getIn o) (f cols)
withTableLike f (pos :< o@(Dict pairs))   = jump pos $ dive (getIn o) (mapM (\(k :*: v) -> (k :|:) <$> withArr return v) pairs >>= f)
withTableLike _ (pos :< other) = jump pos ( expectationError (Id $ "Table{Class=*}") other <|>
                                            expectationError (Id $ "Dict") other )

withColumn :: String -> ([AnnotatedValue] -> Parser a) -> [AnnotatedColumn] -> Parser a
withColumn key p cols =
  case M.lookup key cols' of
    Nothing -> expectationErrorField (Id $ ":" ++ key)
    Just v  -> dive (InColumn key) (p v)
  where
    cols' = M.fromList $ map (\(k :|: v) -> (k, v)) cols

withColumns :: ([AnnotatedValue] -> Parser a) -> [AnnotatedColumn] -> Parser [(String, a)]
withColumns p = mapM (\(k :|: v) -> (k,) <$> dive (InColumn k) (p v))

withArr :: ([AnnotatedValue] -> Parser a) -> AnnotatedValue -> Parser a
withArr f (pos :< o@(Arr vs)) = jump pos $ dive (getIn o) (f vs)
withArr _ (pos :< other)      = jump pos $ expectationError (Id "Arr") other

withElem :: Int -> (AnnotatedValue -> Parser a) -> [AnnotatedValue] -> Parser a
withElem n p vs | n < length vs = dive (AtIndex n) (p (vs !! n))
                | otherwise     = expectationErrorField (Id $ "[" ++ show n ++ "]")

withElems :: (AnnotatedValue -> Parser a) -> [AnnotatedValue] -> Parser [a]
withElems p vs = mapM (\(n,v) -> dive (AtIndex n) (p v)) $ zip [0..] vs

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
