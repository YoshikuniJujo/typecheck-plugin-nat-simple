{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Constraint (
	Constraint, equal, greatEqualThan, greatThan, vars, hasVar,
	isDerivFrom, rmNegative, selfContained, eliminate,
	Poly, (.+), (.-) ) where

import Prelude hiding (null, filter, (<>))

import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Maybe (isJust)
import Data.Map.Strict (Map, null, singleton, (!?), filter, lookupMin)
import Data.Map.Merge.Strict (
	merge, preserveMissing, mapMissing,
	zipWithMatched, zipWithMaybeMatched )

import qualified Data.Map.Strict as M (toList)

---------------------------------------------------------------------------

-- * CONSTRAINT
--	+ DATA CONSTRAINT AND CONSTRUCTOR
-- 	+ VARS, HAS VAR, REMOVE NEGATIVE, IS DERIVE FROM AND SELF CONTAINED
-- 	+ ELIMINATE
-- * POLYNOMIAL

---------------------------------------------------------------------------
-- CONSTRAINT
---------------------------------------------------------------------------

-- DATA CONSTRAINT AND CONSTRUCTOR

data Constraint v = Eq (Poly v) | Geq (Poly v) deriving (Show, Eq, Ord)

equal :: Ord v => Poly v -> Poly v -> Constraint v
l `equal` r = Eq . formatEq $ l .- r

greatEqualThan :: Ord v => Poly v -> Poly v -> Constraint v
l `greatEqualThan` r = Geq . formatGeq $ l .- r

greatThan :: Ord v => Poly v -> Poly v -> Constraint v
l `greatThan` r = Geq $ formatGeq (l .- r) .- singleton Nothing 1

formatEq :: Poly v -> Poly v
formatEq p =
	maybe p ((p `divide` divisor p `times`) . signum . snd) $ lookupMin p

formatGeq :: Poly v -> Poly v
formatGeq p = p `divide` divisor p

times, divide :: Poly v -> Integer -> Poly v
p `times` n = (* n) <$> p
p `divide` n = (`div` n) <$> p

divisor :: Poly v -> Integer
divisor = gcdAll . toList where gcdAll = \case [] -> 1; n : ns -> foldr gcd n ns

-- VARS, HAS VAR, REMOVE NEGATIVE, IS DERIVE FROM AND SELF CONTAINED

vars :: Ord v => Constraint v -> [Maybe v]
vars (Eq p) = (fst <$>) $ M.toList p
vars (Geq p) = (fst <$>) $ M.toList p

hasVar :: Ord v => Constraint v -> Maybe v -> Bool
hasVar (Eq p) v = isJust $ p !? v
hasVar (Geq p) v = isJust $ p !? v

rmNegative :: Constraint v -> Constraint v
rmNegative = \case eq@(Eq _) -> eq; Geq p -> Geq $ filter (>= 0) p

isDerivFrom :: Ord v => Constraint v -> Constraint v -> Bool
Eq w `isDerivFrom` Eq g = w == g
Geq w `isDerivFrom` Geq g = w `isGeqThan` g
_ `isDerivFrom` _ = False

isGeqThan :: Ord v => Poly v -> Poly v -> Bool
l `isGeqThan` r = and $ merge
	(mapMissing \_ nl -> nl >= 0)
	(mapMissing \_ nr -> nr <= 0) (zipWithMatched $ const (>=)) l r

selfContained :: Constraint v -> Bool
selfContained = \case Eq p -> null p; Geq p -> and $ (>= 0) <$> p

-- ELIMINATE

eliminate ::
	Ord v => Constraint v -> Constraint v -> Maybe v -> Maybe (Constraint v)
eliminate (Eq l) (Eq r) v = Eq . formatEq . uncurry (.+) <$> alignEE l r v
eliminate (Eq l) (Geq r) v = Geq . formatGeq . uncurry (.+) <$> alignEG l r v
eliminate (Geq l) (Geq r) v = Geq . formatGeq . uncurry (.+) <$> alignGG l r v
eliminate l r v = eliminate r l v

type Aligned v = Maybe (Poly v, Poly v)

alignEE :: Ord v => Poly v -> Poly v -> Maybe v -> Aligned v
alignEE l r v = (<$> ((,) <$> l !? v <*> r !? v)) \(nl, nr) ->
	(l `times` nr, r `times` (- nl))

alignEG :: Ord v => Poly v -> Poly v -> Maybe v -> Aligned v
alignEG l r v = (<$> ((,) <$> l !? v <*> r !? v)) \(nl, nr) ->
	(l `times` (- signum nl * nr), r `times` abs nl)

alignGG :: Ord v => Poly v -> Poly v -> Maybe v -> Aligned v
alignGG l r v = (,) <$> l !? v <*> r !? v >>= \(nl, nr) -> do
	guard $ nl * nr < 0
	pure (l `times` abs nr, r `times` abs nl)

---------------------------------------------------------------------------
-- POLYNOMIAL
---------------------------------------------------------------------------

type Poly v = Map (Maybe v) Integer

(.+), (.-) :: Ord v => Poly v -> Poly v -> Poly v
(.+) = merge preserveMissing preserveMissing
	(zipWithMaybeMatched \_ a b -> rmZero $ a + b)
(.-) = merge preserveMissing (mapMissing $ const negate)
	(zipWithMaybeMatched \_ a b -> rmZero $ a - b)

rmZero :: (Eq n, Num n) => n -> Maybe n
rmZero = \case 0 -> Nothing; n -> Just n
