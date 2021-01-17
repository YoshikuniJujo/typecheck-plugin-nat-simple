{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.UnNomEq (
	unNomEq, ctEqual, zero, true, false, var, cnst, leq, add, sub ) where

import GhcPlugins (
	PredTree(..), EqRel(..), classifyPredType, ppr, (<+>), text,
	mkPrimEqPred, promotedFalseDataCon, promotedTrueDataCon, Var,
	)
-- import TcRnTypes (Ct, ctEvidence, ctEvPred)
import Control.Monad.Try (Try, throw)
import Data.Log (IsSDoc, fromSDoc)

import TcPluginM
import TcRnTypes
import TyCoRep
import TcTypeNats

unNomEq :: (Monoid w, IsSDoc e) => Ct -> Try e w (Type, Type)
unNomEq ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq l r -> pure (l, r)
	_ -> throw . fromSDoc
		$ text "unNomEq: no match to EqPred NomEq l r:" <+> ppr ct

ctEqual :: Ct -> Type -> Type -> TcPluginM Ct
ctEqual ct l r = do
	hl <- newCoercionHole pt
	pure . mkNonCanonical $ CtWanted pt (HoleDest hl) WDeriv (ctLoc ct)
	where pt = mkPrimEqPred l r

zero :: Type
zero = LitTy $ NumTyLit 0

true :: Type
true = TyConApp promotedTrueDataCon []

false :: Type
false = TyConApp promotedFalseDataCon []

var :: Var -> Type
var = TyVarTy

cnst :: Integer -> Type
cnst = LitTy . NumTyLit

leq, add, sub :: Type -> Type -> Type
l `leq` r = TyConApp typeNatLeqTyCon [l, r]
l `add` r = TyConApp typeNatAddTyCon [l, r]
l `sub` r = TyConApp typeNatSubTyCon [l, r]
