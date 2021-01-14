{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.TypeCheckWith (
	-- * TYPE CHECK WITH
	typeCheckWith ) where

import GhcPlugins (
	Plugin(..), defaultPlugin, Expr(..), mkUnivCo, Role(..),
	Outputable, ppr, text )
import TcPluginM (TcPluginM, tcPluginTrace)
import TcRnTypes (TcPlugin(..), Ct, TcPluginResult(..))
import TcEvidence (EvTerm(..))
import TyCoRep (UnivCoProvenance(..))
import Control.Monad.Try (Try, gatherSuccess, throw)
import Data.Bool (bool)
import Data.Log (IsSDoc, fromSDoc)
import Plugin.TypeCheck.Nat.Simple.UnNomEq (unNomEq)

---------------------------------------------------------------------------

typeCheckWith :: (Monoid w, Outputable w, IsSDoc w) =>
	String -> ([Ct] -> [Ct] -> Ct -> Try w w Bool) -> Plugin
typeCheckWith hd ck = defaultPlugin { tcPlugin = const $ Just TcPlugin {
	tcPluginInit = pure (),
	tcPluginSolve = const $ solve hd ck,
	tcPluginStop = const $ pure () } }

solve :: (Monoid w, Outputable w, IsSDoc w) =>
	String -> ([Ct] -> [Ct] -> Ct -> Try w w Bool) ->
	[Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve hd ck gs ds ws = TcPluginOk rs [] <$ tcPluginTrace hd (ppr lgs)
	where (rs, lgs) = gatherSuccess $ result hd ck gs ds <$> ws

result :: (Monoid s, IsSDoc e) => String ->
	([Ct] -> [Ct] -> Ct -> Try e s Bool) ->
	[Ct] -> [Ct] -> Ct -> Try e s (EvTerm, Ct)
result hd ck gs ds w = unNomEq w >>= \(l, r) ->
	bool (throw em) (pure (et l r, w)) =<< ck gs ds w
	where
	em = fromSDoc $ text "result: type checker: return False"
	et = ((EvExpr . Coercion) .) . mkUnivCo (PluginProv hd) Nominal
