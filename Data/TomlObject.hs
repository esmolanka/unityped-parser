{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.TomlObject where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Applicative
import Control.UnitypedParser
import Control.UnitypedParser.Monad ()
import Control.Parsing

import Data.Functor.Foldable (Fix (..))
import Data.Int
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Traversable as Tr

import Text.Parsec.Error (ParseError)
import Text.Toml
import Text.Toml.Types

import Data.Object.Types
import Data.Object.Parse

data Scalar
  = SString Text
  | SInteger Int64
  | SFloat Double
  | SBoolean Bool
  | SDatetime UTCTime
    deriving (Show, Eq, Ord)

instance GetId Scalar where
  getId (SString _) = Id "String"
  getId (SInteger _) = Id "Integer"
  getId (SFloat _) = Id "Float"
  getId (SBoolean _) = Id "Boolean"
  getId (SDatetime _) = Id "Datetime"

type TomlObject = Object Text Scalar
type AnnotatedTomlObject = AnnotatedObject Text Scalar

instance FieldKey Text where
  fieldQualifier s = InField (T.unpack s)
  fieldIdentifier s = Id $ "." ++ (T.unpack s)

withInteger :: (Int64 -> ParseM a) -> AnnotatedTomlObject -> ParseM a
withInteger p = withScalar go
  where
    go (Right (SInteger n)) = p n
    go other = expectationErrorStr (Id "Integer") . either id getId $ other

withFloat :: (Double -> ParseM a) -> AnnotatedTomlObject -> ParseM a
withFloat p = withScalar go
  where
    go (Right (SFloat n)) = p n
    go other = expectationErrorStr (Id "Float") . either id getId $ other

withString :: (Text -> ParseM a) -> AnnotatedTomlObject -> ParseM a
withString p = withScalar go
  where
    go (Right (SString s)) = p s
    go other = expectationErrorStr (Id "String") . either id getId $ other

withBool :: (Bool -> ParseM a) -> AnnotatedTomlObject -> ParseM a
withBool p = withScalar go
  where
    go (Right (SBoolean s)) = p s
    go other = expectationErrorStr (Id "Bool") . either id getId $ other

class FromTOML a where
  parseTOML :: AnnotatedTomlObject -> ParseM a

instance FromTOML AnnotatedTomlObject where
  parseTOML = pure

instance FromTOML TomlObject where
  parseTOML = pure . unannotate

instance FromTOML Int where
  parseTOML v = fromIntegral <$> withInteger pure v

instance FromTOML Text where
  parseTOML = withString pure

instance FromTOML Bool where
  parseTOML = withBool pure

instance FromTOML a => FromTOML [a] where
  parseTOML = withArray (withElems parseTOML)

instance FromTOML a => FromTOML (M.Map Text a) where
  parseTOML = withObject (Tr.traverse parseTOML)

----------------------------------------------------------------------
-- Parsing

transformTable :: Table -> Object Text Scalar
transformTable = Fix . Object . M.fromList . HM.toList . HM.map transformNode

transformNode :: Node -> Object Text Scalar
transformNode node =
  case node of
    NTValue scalar -> transformScalar scalar
    NTable table   -> transformTable table
    NTArray tables -> Fix $ Array $ map transformTable tables

transformScalar :: TValue -> Object Text Scalar
transformScalar tval = Fix $
  case tval of
    VString str   -> Scalar $ SString str
    VInteger int  -> Scalar $ SInteger int
    VFloat dbl    -> Scalar $ SFloat dbl
    VBoolean bool -> Scalar $ SBoolean bool
    VDatetime ts  -> Scalar $ SDatetime ts
    VArray values -> Array  $ map transformScalar values

parseObject :: FilePath -> Text -> Either ParseError (Object Text Scalar)
parseObject identifier body =
  transformTable <$> parseTomlDoc identifier body

readObjectFromFile :: FilePath -> IO (Either ParseError (Object Text Scalar))
readObjectFromFile filepath =
  parseObject filepath <$> T.readFile filepath

----------------------------------------------------------------------
-- Better combinators

bool :: Parser ParseM AnnotatedTomlObject Bool
bool = mkParser withBool

int :: Parser ParseM AnnotatedTomlObject Int64
int = mkParser withInteger

double :: Parser ParseM AnnotatedTomlObject Double
double = mkParser withFloat

string :: Parser ParseM AnnotatedTomlObject Text
string = mkParser withString

index :: Int -> Parser ParseM AnnotatedTomlObject AnnotatedTomlObject
index n = mkParser (\f -> withArray (withElem n f))

elems :: Parser ParseM AnnotatedTomlObject [AnnotatedTomlObject]
elems = mkParser withArray

key :: Text -> Parser ParseM AnnotatedTomlObject AnnotatedTomlObject
key k = mkParser (\f -> withObject (withField k f))

fields :: Parser ParseM AnnotatedTomlObject (M.Map Text AnnotatedTomlObject)
fields = mkParser withObject

instance Hole ParseM AnnotatedTomlObject where
  holes = elems <|> (fields >>> arr M.elems) <|> pure []
