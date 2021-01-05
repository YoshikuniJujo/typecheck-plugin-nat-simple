{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.PluginWith (pluginWith) where

import GhcPlugins
import TcRnTypes
import TyCoRep
import TcEvidence
import TcPluginM

import Control.Monad.Trans.Except
import Data.Bool
import Data.Either
import Data.Except.Message

import Plugin.TypeCheck.Nat.Simple.UnNomEq

pluginWith :: ([Ct] -> [Ct] -> Ct -> Except Message Bool) -> Plugin
pluginWith ck = defaultPlugin { tcPlugin = const $ Just TcPlugin {
	tcPluginInit = pure (),
	tcPluginSolve = const $ solve ck,
	tcPluginStop = const $ pure () } }

solve :: ([Ct] -> [Ct] -> Ct -> Except Message Bool) -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve _ _ _ [] = pure $ TcPluginOk [] []
solve ck gs ds ws = do
	tcPluginTrace "!Plugin.TypeCheck.Nat.Simple" ""
--	tcPluginTrace "Given: " . ppr $ runExcept . decode <$> gs
--	tcPluginTrace "Derived: " . ppr $ runExcept . decode <$> ds
--	tcPluginTrace "Wanted: " . ppr $ runExcept . decode <$> ws
	pure $ TcPluginOk (rights $ runExcept . result ck gs ds <$> ws) []

result :: ([Ct] -> [Ct] -> Ct -> Except Message Bool) -> [Ct] -> [Ct] -> Ct -> Except Message (EvTerm, Ct)
result ck gs ds w = unNomEq w >>= \(l, r) -> bool (throwE em) (pure (et l r, w)) =<< ck gs ds w
	where
	em = "result: fail"
	et l r = EvExpr . Coercion $
		mkUnivCo (PluginProv "Plugin.TypeCheck.Nat.Simple") Nominal l r
