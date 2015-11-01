{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.UnitypedParser.Monad where

import Control.Monad.Reader
import Control.Monad.Error

import Control.UnitypedParser.Parser

instance Monad Result where
  return = Success
  (Success a) >>= fm = fm a
  Failure f >>= _ = Failure f

deriving instance Monad ParseM
deriving instance (MonadReader (Position, [Context])) ParseM
instance MonadError FailureTree ParseM where
  throwError fs = mkParseM (Failure . settle fs . reverse . fst)
  catchError p f = do
    env <- ask
    case runParseM' env p of
      Success a -> return a
      Failure fs -> f fs
