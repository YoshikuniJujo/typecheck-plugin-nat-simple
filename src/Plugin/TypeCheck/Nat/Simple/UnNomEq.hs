{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.UnNomEq where

import GhcPlugins
import TcRnTypes

-- import Control.Monad.Trans.Except
-- import Data.Except.Message

import Control.Monad.Try
import Data.String

unNomEq :: (Monoid s, IsString s) => Ct -> Try s (Type, Type)
unNomEq ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq l r -> pure (l, r)
	_ -> throw "Cannot unNomEq"
