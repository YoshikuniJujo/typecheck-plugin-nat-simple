{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.UnNomEq (unNomEq) where

import Constraint (Ct, ctEvidence, ctEvPred)
import Type (Type)
import Predicate(Pred(..), EqRel(..), classifyPredType)
import Outputable (ppr, text, (<+>))
import Control.Monad.Try (Try, throw)
import Data.Log (IsSDoc, fromSDoc)

unNomEq :: (Monoid w, IsSDoc e) => Ct -> Try e w (Type, Type)
unNomEq ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq l r -> pure (l, r)
	_ -> throw . fromSDoc
		$ text "unNomEq: no match to EqPred NomEq l r:" <+> ppr ct
