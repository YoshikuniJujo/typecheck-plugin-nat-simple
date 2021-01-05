{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple (
	-- * PLUGIN
	plugin,
	-- * PLUGIN WITH
	pluginWith, check, decode, unNomEq ) where

import GhcPlugins
import TcPluginM
import TcRnTypes

import Control.Monad.Trans.Except
import Control.Monad

import Plugin.TypeCheck.Nat.Simple.Decode

import TcEvidence
import TyCoRep
import Data.Bool
import Data.Either

import Data.Derivation.CanDerive
import Data.Except.Message

plugin :: Plugin
plugin = pluginWith check

pluginWith :: ([Ct] -> [Ct] -> Ct -> Except Message Bool) -> Plugin
pluginWith ck = defaultPlugin { tcPlugin = const $ Just TcPlugin {
	tcPluginInit = pure (),
	tcPluginSolve = const $ solve ck,
	tcPluginStop = const $ pure () } }

check :: [Ct] -> [Ct] -> Ct -> Except Message Bool
check gs _ds w = unNomEq w >>= \(l, r) -> canDerive g <$> (wnt =<< decode l r)
	where
	g = given . rights $ runExcept . (uncurry decode <=< unNomEq) <$> gs
	wnt = maybe (throwE "wanted: fail") pure . wanted

-- canDerive' ::

solve :: ([Ct] -> [Ct] -> Ct -> Except Message Bool) -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve _ _ _ [] = pure $ TcPluginOk [] []
solve ck gs ds ws = do
	tcPluginTrace "!Plugin.TypeCheck.Nat.Simple" ""
	tcPluginTrace "Given: " . ppr $ runExcept . (uncurry decode <=< unNomEq) <$> gs
	tcPluginTrace "Derived: " . ppr $ runExcept . (uncurry decode <=< unNomEq) <$> ds
	tcPluginTrace "Wanted: " . ppr $ runExcept . (uncurry decode <=< unNomEq) <$> ws
	pure $ TcPluginOk (rights $ runExcept . result ck gs ds <$> ws) []

unNomEq :: Ct -> Except Message (Type, Type)
unNomEq ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq l r -> pure (l, r)
	_ -> throwE "Cannot unNomEq"

result :: ([Ct] -> [Ct] -> Ct -> Except Message Bool) -> [Ct] -> [Ct] -> Ct -> Except Message (EvTerm, Ct)
result ck gs ds w = unNomEq w >>= \(l, r) -> bool (throwE em) (pure (et l r, w)) =<< ck gs ds w
	where
	em = "result: fail"
	et l r = EvExpr . Coercion $
		mkUnivCo (PluginProv "Plugin.TypeCheck.Nat.Simple") Nominal l r
