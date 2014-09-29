{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Object.JsonObject where

import Control.Applicative
import Control.Monad.UnitypedParser
import Control.Monad.UnitypedParser.Monad ()

import qualified Data.Traversable as Tr
import Data.Scientific
import qualified Data.Map as M

import Data.Object.Types
import Data.Object.Parse
import Data.Object.Combinators

data Scalar
  = Number Scientific
  | Str String
  | Logic Bool
  | Undefined
    deriving (Show, Eq, Ord)

instance GetId Scalar where
  getId (Number _) = Id "Number"
  getId (Str _) = Id "Str"
  getId (Logic _) = Id "Bool"
  getId (Undefined) = Id "Undefined"

type JsonObject = Object String Scalar
type AnnotatedJsonObject = AnnotatedObject String Scalar

instance FieldKey String where
  fieldQualifier s = InField s
  fieldIdentifier s = Id $ "." ++ s

object :: [Pair String Scalar] -> Object String Scalar
object = mkObject

mkNumber :: Scientific -> JsonObject
mkNumber = mkScalar . Number

mkStr :: String -> JsonObject
mkStr = mkScalar . Str

mkBool :: Bool -> JsonObject
mkBool = mkScalar . Logic

mkUndefined :: JsonObject
mkUndefined = mkScalar Undefined

pNumber :: AnnotatedJsonObject -> ParseM Scientific
pNumber = withScalar go
  where
    go (Right (Number n)) = pure n
    go other = expectationErrorStr (Id "Number") . either id getId $ other

pStr :: AnnotatedJsonObject -> ParseM String
pStr = withScalar go
  where
    go (Right (Str s)) = pure s
    go other = expectationErrorStr (Id "Str") . either id getId $ other

pBool :: AnnotatedJsonObject -> ParseM Bool
pBool = withScalar go
  where
    go (Right (Logic s)) = pure s
    go other = expectationErrorStr (Id "Bool") . either id getId $ other

pUndefined :: AnnotatedJsonObject -> ParseM ()
pUndefined = withScalar go
  where
    go (Right Undefined) = pure ()
    go other = expectationErrorStr (Id "Undefined") . either id getId $ other

class FromJSON a where
  parseJSON :: AnnotatedJsonObject -> ParseM a

class ToJSON a where
  toJSON :: a -> JsonObject

instance FromJSON AnnotatedJsonObject where
  parseJSON = pure

instance FromJSON JsonObject where
  parseJSON = pure . unannotate

instance ToJSON JsonObject where
  toJSON = id

instance FromJSON Int where
  parseJSON v = fromIntegral . truncate <$> pNumber v

instance ToJSON Int where
  toJSON = mkNumber . fromIntegral

instance FromJSON String where
  parseJSON = pStr

instance ToJSON String where
  toJSON = mkStr

instance FromJSON Bool where
  parseJSON = pBool

instance ToJSON Bool where
  toJSON = mkBool

instance FromJSON a => FromJSON [a] where
  parseJSON = withArray (withElems parseJSON)

instance (ToJSON a) => ToJSON [a] where
  toJSON = mkArray . map toJSON

instance FromJSON a => FromJSON (M.Map String a) where
  parseJSON = withObject (Tr.traverse parseJSON)

instance (ToJSON a) => ToJSON (M.Map String a) where
  toJSON = mkObject' . M.map toJSON

instance GettableByKey FromJSON String (M.Map String (AnnotatedJsonObject)) where
  (.:) pairs key = withField key parseJSON pairs

instance GettableByKey FromJSON String AnnotatedJsonObject where
  (.:) d key = withObject (withField key parseJSON) d

instance (a ~ AnnotatedJsonObject) => GettableByKey FromJSON String (ParseM a) where
  (.:) ppairs key = ppairs >>= withObject (withField key parseJSON)

instance Indexable FromJSON AnnotatedJsonObject where
  (.!!) arr n = withArray (withElem n parseJSON) arr

instance Indexable FromJSON [AnnotatedJsonObject] where
  (.!!) els n = withElem n parseJSON els

instance (a ~ AnnotatedJsonObject) => Indexable FromJSON (ParseM a) where
  (.!!) pval n = pval >>= withArray (withElem n parseJSON)

instance BuildableByKey ToJSON String JsonObject where
  (.=) key value = key :*: toJSON value
