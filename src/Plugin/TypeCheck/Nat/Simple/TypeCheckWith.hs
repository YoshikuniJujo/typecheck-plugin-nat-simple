{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.TypeCheckWith (
	-- * TYPE CHECK WITH
	typeCheckWith ) where

import GhcPlugins ( Var,
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

import Data.Result hiding (result)
import qualified Data.Result as R

import Data.Derivation.Expression
import Plugin.TypeCheck.Nat.Simple.Encode

---------------------------------------------------------------------------

typeCheckWith :: (Monoid w, Outputable w, IsSDoc w) =>
	String -> ([Ct] -> [Ct] -> Ct -> Try w w (Result [Exp Var 'Boolean])) -> Plugin
typeCheckWith hd ck = defaultPlugin { tcPlugin = const $ Just TcPlugin {
	tcPluginInit = pure (),
	tcPluginSolve = const $ solve hd ck,
	tcPluginStop = const $ pure () } }

solve :: (Monoid w, Outputable w, IsSDoc w) =>
	String -> ([Ct] -> [Ct] -> Ct -> Try w w (Result [Exp Var 'Boolean])) ->
	[Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve hd ck gs ds ws = do
	cts <- uncurry encode `mapM` concat ws'
	TcPluginOk rs cts <$ tcPluginTrace hd (ppr lgs)
	where
	(rws, lgs) = gatherSuccess $ result hd ck gs ds <$> ws
	(rs, ws') = unzip rws

result :: (Monoid s, IsSDoc e) => String ->
	([Ct] -> [Ct] -> Ct -> Try e s (Result [Exp Var 'Boolean])) ->
	[Ct] -> [Ct] -> Ct -> Try e s ((EvTerm, Ct), [(Ct, Exp Var 'Boolean)])
result hd ck gs ds w = unNomEq w >>= \(l, r) ->
	R.result (pure ((et l r, w), [])) (pure . ((et l r, w) ,) . map (w ,)) =<< ck gs ds w
	where
--	em = fromSDoc $ text "result: type checker: return False"
	et = ((EvExpr . Coercion) .) . mkUnivCo (PluginProv hd) Nominal
	more l r e = do
		w' <- encode w `mapM` e
		pure ((et l r, w), w')
