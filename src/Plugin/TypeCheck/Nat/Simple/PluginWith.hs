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

pluginWith :: String -> ([Ct] -> [Ct] -> Ct -> Try SDocStr SDocStr Bool) -> Plugin
pluginWith hd ck = defaultPlugin { tcPlugin = const $ Just TcPlugin {
	tcPluginInit = pure (),
	tcPluginSolve = const $ solve hd ck,
	tcPluginStop = const $ pure () } }

solve :: String -> ([Ct] -> [Ct] -> Ct -> Try SDocStr SDocStr Bool) -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
-- solve _ _ _ _ [] = pure $ TcPluginOk [] []
solve hd ck gs ds ws = do
--	tcPluginTrace "Given: " . ppr $ runExcept . decode <$> gs
--	tcPluginTrace "Derived: " . ppr $ runExcept . decode <$> ds
--	tcPluginTrace "Wanted: " . ppr $ runExcept . decode <$> ws
--	let	(rtns, lgs) = unzip $ runTry . result ck gs ds <$> ws
	let	(rtns, lgs) = gatherSuccess $ result hd ck gs ds <$> ws
--	tcPluginTrace "!Plugin.TypeCheck.Nat.Simple" $ ppr lgs
	tcPluginTrace hd $ ppr lgs
	pure $ TcPluginOk rtns []
--	pure $ TcPluginOk (rights rtns) []
--	pure $ TcPluginOk (rights . map fst $ runTry . result ck gs ds <$> ws) []

result :: (Monoid s, IsString e) => String -> ([Ct] -> [Ct] -> Ct -> Try e s Bool) -> [Ct] -> [Ct] -> Ct -> Try e s (EvTerm, Ct)
result hd ck gs ds w = unNomEq w >>= \(l, r) -> bool (throw em) (pure (et l r, w)) =<< ck gs ds w
	where
	em = "result: fail"
	et l r = EvExpr . Coercion $
		mkUnivCo (PluginProv hd) Nominal l r
