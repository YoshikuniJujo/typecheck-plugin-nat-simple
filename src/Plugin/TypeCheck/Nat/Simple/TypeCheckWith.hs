{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.TypeCheckWith (
	-- * TYPE CHECK WITH
	typeCheckWith ) where

import GhcPlugins (Plugin(..), defaultPlugin, Expr(..), mkUnivCo)
import TcPluginM (TcPluginM, tcPluginTrace)
import TcRnTypes (TcPlugin(..), Ct, TcPluginResult(..))
import TcEvidence (EvTerm(..), Role(..))
import TyCoRep (UnivCoProvenance(..))
import Control.Monad.Try (Try, gatherSuccess, throw)
import Data.Bool (bool)

import Plugin.TypeCheck.Nat.Simple.UnNomEq (unNomEq)

import Outputable
import Data.Log

---------------------------------------------------------------------------

typeCheckWith :: (Monoid w, Outputable w, IsSDoc w) =>
	String -> ([Ct] -> [Ct] -> Ct -> Try w w Bool) -> Plugin
typeCheckWith hd ck = defaultPlugin { tcPlugin = const $ Just TcPlugin {
	tcPluginInit = pure (),
	tcPluginSolve = const $ solve hd ck,
	tcPluginStop = const $ pure () } }

solve :: (Monoid w, Outputable w, IsSDoc w) => String -> ([Ct] -> [Ct] -> Ct -> Try w w Bool) ->
	[Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve hd ck gs ds ws = do
	let	(rtns, lgs) = gatherSuccess $ result hd ck gs ds <$> ws
	tcPluginTrace hd $ ppr lgs
	pure $ TcPluginOk rtns []

result :: (Monoid s, IsSDoc e) => String ->
	([Ct] -> [Ct] -> Ct -> Try e s Bool) ->
	[Ct] -> [Ct] -> Ct -> Try e s (EvTerm, Ct)
result hd ck gs ds w = unNomEq w >>= \(l, r) ->
	bool (throw em) (pure (et l r, w)) =<< ck gs ds w
	where
	em = fromSDoc $ text "result: type checker: return False"
	et l r = EvExpr . Coercion $ mkUnivCo (PluginProv hd) Nominal l r
