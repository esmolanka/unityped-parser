{-# LANGUAGE TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , FunctionalDependencies
  #-}
module Data.Object.Parse where

import Control.Comonad.Cofree

import qualified Data.Map as M
import qualified Data.Traversable as Tr

import Control.Monad.UnitypedParser
import Data.Object.Types

withObject
  :: (GetId s)
  => (M.Map k (AnnotatedObject k s) -> ParseM a)
  -> AnnotatedObject k s
  -> ParseM a
withObject f (pos :< o@(Object pairs)) = jump pos $ dive (getIn o) (f pairs)
withObject _ (pos :< other)            = jump pos $ expectationError (Id "Object") other

withField
  :: (FieldKey k)
  => k
  -> (AnnotatedObject k s -> ParseM a)
  -> M.Map k (AnnotatedObject k s)
  -> ParseM a
withField key p pairs =
  case M.lookup key pairs of
    Nothing -> expectationErrorField (fieldIdentifier key)
    Just v  -> dive (fieldQualifier key) (p v)

withFields
  :: (FieldKey k)
  => (k -> AnnotatedObject k s -> ParseM a)
  -> M.Map k (AnnotatedObject k s)
  -> ParseM (M.Map k a)
withFields p = Tr.sequence . M.mapWithKey (\k -> dive (fieldQualifier k) . p k)

withArray
  :: (GetId s)
  => ([AnnotatedObject k s] -> ParseM a)
  -> AnnotatedObject k s
  -> ParseM a
withArray f (pos :< o@(Array vs)) = jump pos $ dive (getIn o) (f vs)
withArray _ (pos :< other)        = jump pos $ expectationError (Id "Array") other

withElem
  :: Int
  -> (AnnotatedObject k s -> ParseM a)
  -> [AnnotatedObject k s]
  -> ParseM a
withElem n p vs | n < length vs = dive (AtIndex n) (p (vs !! n))
                | otherwise     = expectationErrorField (Id $ "[" ++ show n ++ "]")

withElems
  :: (AnnotatedObject k s -> ParseM a)
  -> [AnnotatedObject k s]
  -> ParseM [a]
withElems p vs = mapM (\(n,v) -> dive (AtIndex n) (p v)) $ zip [0..] vs

withScalar
  :: (GetId s)
  => (Either Identifier s -> ParseM a)
  -> AnnotatedObject k s
  -> ParseM a
withScalar f (pos :< Scalar s) = jump pos $ f (Right s)
withScalar f (pos :< other)    = jump pos $ f (Left $ getId other)

