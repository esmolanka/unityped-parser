{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Unityped.Better where

import Control.Applicative
import Control.Arrow

import Data.Unityped.Value
import qualified Control.Monad.UnitypedParser.Parser as P
import Control.Monad.UnitypedParser.Better

import Control.Parsing

bool :: UnitypedParser AnnotatedValue Bool
bool = mkParser withBool

int :: UnitypedParser AnnotatedValue Int
int = mkParser withInt

double :: UnitypedParser AnnotatedValue Double
double = mkParser withDouble

string :: UnitypedParser AnnotatedValue String
string = mkParser withStr

index :: Int -> UnitypedParser AnnotatedValue AnnotatedValue
index n = mkParser (\f -> withArr (withElem n f))

elems :: UnitypedParser AnnotatedValue [AnnotatedValue]
elems = mkParser withArr

key :: String -> UnitypedParser AnnotatedValue AnnotatedValue
key k = mkParser (\f -> withDict (withField k f))

fields :: UnitypedParser AnnotatedValue [AnnotatedPair]
fields = mkParser withDict

instance Hole P.ParseM AnnotatedValue where
  holes = elems <|> (fields >>> arr (map (\(_ :*: v) -> v))) <|> pure []
