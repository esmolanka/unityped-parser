{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TypeFamilies #-}

module Data.Object.Types where

import Control.Applicative
import Control.Monad.Reader
import Control.Comonad.Cofree

import Data.Functor.Foldable (Fix (..), cata)
import qualified Data.Map as M

import qualified Data.Foldable as Fo
import qualified Data.Traversable as Tr

import Control.UnitypedParser

data ObjectF k s e
  = Object !(M.Map k e)
  | Array ![e]
  | Scalar !s
    deriving (Show, Eq, Functor, Tr.Traversable, Fo.Foldable)

data PairF k e = k :*: e
  deriving (Eq, Show, Functor, Tr.Traversable, Fo.Foldable)

mkObject' :: (FieldKey k) => M.Map k (Object k s) -> Object k s
mkObject' = Fix . Object

mkObject :: (FieldKey k) => [PairF k (Object k s)] -> Object k s
mkObject = Fix . Object . M.fromList . map (\(k :*: v) -> (k, v))

mkArray :: [Object k s] -> Object k s
mkArray = Fix . Array

mkScalar :: s -> Fix (ObjectF k s)
mkScalar = Fix . Scalar

type Object k s = Raw (ObjectF k s)
type AnnotatedObject k s = Annotated (ObjectF k s)

type Pair k s = PairF k (Object k s)
type AnnotatedPair k s = PairF k (AnnotatedObject k s)

annotateWithIndices
  :: [Reader Position (AnnotatedObject k s)]
  -> Reader Position [AnnotatedObject k s]
annotateWithIndices = mapM (\(i, r) -> local (AtIndex i:) r) . zip [0..]

annotateField
  :: (FieldKey k)
  => M.Map k (Reader Position (AnnotatedObject k s))
  -> Reader Position (M.Map k (AnnotatedObject k s))
annotateField = Tr.sequence . M.mapWithKey (\k -> local (fieldQualifier k:))

instance (GetId s) => GetId (ObjectF k s e)  where
  getId (Object _) = Id "Object"
  getId (Array _)  = Id "Array"
  getId (Scalar s) = getId s

class (Ord k) => FieldKey k where
  fieldQualifier :: k -> Qualifier
  fieldIdentifier :: k -> Identifier

instance (FieldKey k, GetId s) => WithAnnotation (ObjectF k s) where
  annotate root = runReader (cata alg root) [InObj (Id "@")]
    where
      alg :: (FieldKey k, GetId s) =>
             ObjectF k s (Reader Position (Cofree (ObjectF k s) Position))
          -> Reader Position (Cofree (ObjectF k s) Position)
      alg obj@(Object pairs) = (:<) <$> ask <*> local (getIn obj:) (Object <$> annotateField pairs)
      alg obj@(Array vals)   = (:<) <$> ask <*> local (getIn obj:) (Array <$> annotateWithIndices vals)
      alg obj@(Scalar s)     = (:<) <$> ask <*> local (getIn obj:) (Scalar <$> pure s)

  unannotate = cataAnn (const Fix)
