{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TestObj where

import Control.Applicative

import System.IO
import System.Process

import Data.Monoid

import Control.UnitypedParser
import Data.Object
import Data.JsonObject

structureDeep :: Object String Scalar
structureDeep = object [ "Foo" .= object [ "Bar" .= object [ "X" .= mkNumber 5 ] ] ]

deepAlternatives :: AnnotatedJsonObject -> ParseM Int
deepAlternatives v = alt0 v <|> alt1 v <|>  alt2 v <|> alt3 v <|> alt4 v <|> alt5 v
  where
    alt0 v = parseJSON v
    alt1 v = v .: "Foo" .: "Bar"
    alt2 v = v .: "Foo" .: "Bar" .: "X" .!! 5
    alt3 v = v .: "Foo" .: "Baz" .!! 5
    alt4 v = v .: "Foo" .: "Baz" .!! 10
    alt5 v = v .: "Foo" .: "Bar" .: "X" .: "Elems" .!! 0

pX :: AnnotatedJsonObject -> ParseM Int
pX = withObject (\o -> withField "X" parseJSON o
                   <|> withField "Y" parseJSON o)

pBar :: AnnotatedObject String Scalar -> ParseM Int
pBar = withObject (withField "Bar" pFoo)

pFoo :: AnnotatedObject String Scalar -> ParseM Int
pFoo = withObject (withField "Foo" pBar)

pKey :: String -> Endo (AnnotatedObject String Scalar -> ParseM a)
pKey k = Endo (\p -> withObject (withField k p))

pElem :: Int -> Endo (AnnotatedObject String Scalar -> ParseM a)
pElem n = Endo (\p -> withArray (withElem n p))

pFooBarBaz :: Endo (AnnotatedObject String Scalar -> ParseM a)
pFooBarBaz = pKey "Foo" <> pKey "Bar" <> pKey "Baz"

pNumThere :: AnnotatedObject String Scalar -> ParseM Int
pNumThere = appEndo pFooBarBaz parseJSON

--alt5 v = v .: "Foo" .: "Bar" .: "X" .: "Elems" .!! 0

pAlt5 :: AnnotatedObject String Scalar -> (AnnotatedObject String Scalar -> ParseM a) -> ParseM a
pAlt5 = with (mempty <.> "Foo" <.> "Bar" <.> "X" <.> "Elems" .!!! 0)

withPath :: (FromJSON a) => [String] -> AnnotatedJsonObject -> ParseM a
withPath = ($ parseJSON) . appEndo . foldl (<.>) mempty

(.::) :: (FromJSON a) => AnnotatedJsonObject -> [String] -> ParseM a
(.::) = flip withPath

pAlt5' :: AnnotatedObject String Scalar -> ParseM Int
pAlt5' d = d .:: ["Foo", "Bar", "X", "Elems"]

with :: Endo (a -> b) -> a -> (a -> b) -> b
with p = flip (appEndo p)

type EndoParser a = Endo (AnnotatedObject String Scalar -> ParseM a)

(.:::) :: FromJSON a => AnnotatedObject String Scalar -> EndoParser a -> ParseM a
(.:::) o e = appEndo e parseJSON o

withFocus :: FromJSON a => Endo (AnnotatedJsonObject -> ParseM a) -> AnnotatedJsonObject -> ParseM a
withFocus e = appEndo e parseJSON

pAlt5'' :: AnnotatedObject String Scalar -> ParseM Int
pAlt5'' = withFocus (mempty <.> "Foo" <.> "Bar" <.> "X" .!!! 5)

(<.>) :: Endo (AnnotatedObject String Scalar -> ParseM a) -> String -> Endo (AnnotatedObject String Scalar -> ParseM a)
(<.>) p k = p <> pKey k

(.!!!) :: Endo (AnnotatedObject String Scalar -> ParseM a) -> Int -> Endo (AnnotatedObject String Scalar -> ParseM a)
(.!!!) p n = p <> pElem n

putStrPager :: String -> IO ()
putStrPager str = do
  (Just pipe, _, _, process) <-
    createProcess
      (CreateProcess (RawCommand "less" [])
       Nothing Nothing CreatePipe
       (UseHandle stdout) (UseHandle stderr)
       True False True)
  hPutStrLn pipe str
  hClose pipe
  waitForProcess process
  return ()

main :: IO ()
main = do
  putStrLn "Running parser..."
  either putStrPager print $ parsePretty deepAlternatives structureDeep
