{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.PluginWith (pluginWith) where

import GhcPlugins (Plugin(..), defaultPlugin, Expr(..), mkUnivCo, ppr)
import TcPluginM (TcPluginM, tcPluginTrace)
import TcRnTypes (TcPlugin(..), Ct, TcPluginResult(..))
import TcEvidence (EvTerm(..), Role(..))
import TyCoRep (UnivCoProvenance(..))
import Control.Monad.Try (Try, gatherSuccess, throw, SDocStr)
import Data.Bool (bool)
import Data.String (IsString)

import Plugin.TypeCheck.Nat.Simple.UnNomEq (unNomEq)

---------------------------------------------------------------------------

pluginWith ::
	String -> ([Ct] -> [Ct] -> Ct -> Try SDocStr SDocStr Bool) -> Plugin
pluginWith hd ck = defaultPlugin { tcPlugin = const $ Just TcPlugin {
	tcPluginInit = pure (),
	tcPluginSolve = const $ solve hd ck,
	tcPluginStop = const $ pure () } }

solve :: String -> ([Ct] -> [Ct] -> Ct -> Try SDocStr SDocStr Bool) ->
	[Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve hd ck gs ds ws = do
	let	(rtns, lgs) = gatherSuccess $ result hd ck gs ds <$> ws
	tcPluginTrace hd $ ppr lgs
	pure $ TcPluginOk rtns []

result :: (Monoid s, IsString e) => String -> ([Ct] -> [Ct] -> Ct -> Try e s Bool) -> [Ct] -> [Ct] -> Ct -> Try e s (EvTerm, Ct)
result hd ck gs ds w = unNomEq w >>= \(l, r) -> bool (throw em) (pure (et l r, w)) =<< ck gs ds w
	where
	em = "result: fail"
	et l r = EvExpr . Coercion $
		mkUnivCo (PluginProv hd) Nominal l r
