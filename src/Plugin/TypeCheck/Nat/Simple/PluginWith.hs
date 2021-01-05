{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.PluginWith (pluginWith) where

import GhcPlugins
import TcRnTypes
import TyCoRep
import TcEvidence
import TcPluginM

-- import Control.Monad.Trans.Except
import Data.Bool
-- import Data.Except.Message

import Plugin.TypeCheck.Nat.Simple.UnNomEq

import Control.Monad.Try
import Data.String

pluginWith :: ([Ct] -> [Ct] -> Ct -> Try SDocStr Bool) -> Plugin
pluginWith ck = defaultPlugin { tcPlugin = const $ Just TcPlugin {
	tcPluginInit = pure (),
	tcPluginSolve = const $ solve ck,
	tcPluginStop = const $ pure () } }

solve :: ([Ct] -> [Ct] -> Ct -> Try SDocStr Bool) -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve _ _ _ [] = pure $ TcPluginOk [] []
solve ck gs ds ws = do
	tcPluginTrace "!Plugin.TypeCheck.Nat.Simple" ""
--	tcPluginTrace "Given: " . ppr $ runExcept . decode <$> gs
--	tcPluginTrace "Derived: " . ppr $ runExcept . decode <$> ds
--	tcPluginTrace "Wanted: " . ppr $ runExcept . decode <$> ws
--	let	(rtns, lgs) = unzip $ runTry . result ck gs ds <$> ws
	let	(rtns, lgs) = rights $ result ck gs ds <$> ws
	tcPluginTrace "" $ ppr lgs
	pure $ TcPluginOk rtns []
--	pure $ TcPluginOk (rights rtns) []
--	pure $ TcPluginOk (rights . map fst $ runTry . result ck gs ds <$> ws) []

result :: (Monoid s, IsString s) => ([Ct] -> [Ct] -> Ct -> Try s Bool) -> [Ct] -> [Ct] -> Ct -> Try s (EvTerm, Ct)
result ck gs ds w = unNomEq w >>= \(l, r) -> bool (throw em) (pure (et l r, w)) =<< ck gs ds w
	where
	em = "result: fail"
	et l r = EvExpr . Coercion $
		mkUnivCo (PluginProv "Plugin.TypeCheck.Nat.Simple") Nominal l r
