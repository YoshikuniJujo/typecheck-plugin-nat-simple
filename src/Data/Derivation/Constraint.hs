{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Constraint (
	Constraint, equal, greatEqualThan, greatThan, vars, has,
	isDerivFrom, positives, selfContained, eliminate,
	Poly, (.+), (.-) ) where

import Prelude hiding (null, filter, (<>))

import Control.Monad (guard)
import Data.Maybe (isJust)
import Data.Map.Strict (Map, null, singleton, (!?), filter, toList, lookupMin)
import Data.Map.Merge.Strict (
	merge, preserveMissing, mapMissing,
	zipWithMatched, zipWithMaybeMatched )

---------------------------------------------------------------------------

-- * CONSTRAINT
--	+ DATA CONSTRAINT
--	+ CONSTRUCT
--	+ READ
-- 	+ CONVERT
-- * POLYNOMIAL
-- 	+ TYPE POLY
-- 	+ CONSTRUCT
-- 	+ READ
-- 	+ CONVERT

---------------------------------------------------------------------------
-- CONSTRAINT
---------------------------------------------------------------------------

-- DATA CONSTRAINT

data Constraint v = Eq (Poly v) | Geq (Poly v) deriving (Show, Eq, Ord)

-- CONSTRUCT

equal :: Ord v => Poly v -> Poly v -> Constraint v
l `equal` r = Eq . posit . reduce $ l .- r

greatEqualThan :: Ord v => Poly v -> Poly v -> Constraint v
l `greatEqualThan` r = Geq . reduce $ l .- r

greatThan :: Ord v => Poly v -> Poly v -> Constraint v
l `greatThan` r = Geq $ reduce (l .- r) .- singleton Nothing 1

-- READ

vars :: Ord v => Constraint v -> [Maybe v]
vars = \case Eq p -> (fst <$>) $ toList p; Geq p -> (fst <$>) $ toList p

has :: Ord v => Constraint v -> Maybe v -> Bool
has = \case Eq p -> isJust . (p !?); Geq p -> isJust . (p !?)

selfContained :: Constraint v -> Bool
selfContained = \case Eq p -> null p; Geq p -> and $ (>= 0) <$> p

isDerivFrom :: Ord v => Constraint v -> Constraint v -> Bool
Eq w `isDerivFrom` Eq g = w == g
Geq w `isDerivFrom` Eq g = w `isGeqThan` g
Geq w `isDerivFrom` Geq g = w `isGeqThan` g
_ `isDerivFrom` _ = False

-- CONVERT

positives :: Constraint v -> Constraint v
positives = \case eq@(Eq _) -> eq; Geq p -> Geq $ filter (>= 0) p

eliminate ::
	Ord v => Maybe v -> Constraint v -> Constraint v -> Maybe (Constraint v)
eliminate v (Eq l) (Eq r) = Eq . posit . reduce . uncurry (.+) <$> alignEE l r v
eliminate v (Eq l) (Geq r) = Geq . reduce . uncurry (.+) <$> alignEG l r v
eliminate v (Geq l) (Geq r) = Geq . reduce . uncurry (.+) <$> alignGG l r v
eliminate v l r = eliminate v r l

type Aligned v = Maybe (Poly v, Poly v)

alignEE :: Ord v => Poly v -> Poly v -> Maybe v -> Aligned v
alignEE l r v =
	(<$> ((,) <$> l !? v <*> r !? v)) \(m, s) -> (l `mul` s, r `mul` (- m))

alignEG :: Ord v => Poly v -> Poly v -> Maybe v -> Aligned v
alignEG l r v = (<$> ((,) <$> l !? v <*> r !? v)) \(m, s) ->
	(l `mul` (- signum m * s), r `mul` abs m)

alignGG :: Ord v => Poly v -> Poly v -> Maybe v -> Aligned v
alignGG l r v = (,) <$> l !? v <*> r !? v >>= \(m, s) ->
	(l `mul` abs s, r `mul` abs m) <$ guard (m * s < 0)

---------------------------------------------------------------------------
-- POLYNOMIAL
---------------------------------------------------------------------------

-- TYPE POLY

type Poly v = Map (Maybe v) Integer

-- CONSTRUCT

(.+), (.-) :: Ord v => Poly v -> Poly v -> Poly v
(.+) = merge preserveMissing preserveMissing
	(zipWithMaybeMatched \_ a b -> (<$) <$> id <*> guard . (/= 0) $ a + b)
(.-) = merge preserveMissing (mapMissing $ const negate)
	(zipWithMaybeMatched \_ a b -> (<$) <$> id <*> guard . (/= 0) $ a - b)

-- READ

isGeqThan :: Ord v => Poly v -> Poly v -> Bool
isGeqThan = (and .) . merge
	(mapMissing \_ nl -> nl >= 0)
	(mapMissing \_ nr -> nr <= 0) (zipWithMatched $ const (>=))

-- CONVERT

posit :: Poly v -> Poly v
posit p = p `maybe` ((p `mul`) . signum . snd) $ lookupMin p

reduce :: Poly v -> Poly v
reduce = divide <$> id <*> foldr gcd 0

mul, divide :: Poly v -> Integer -> Poly v
mul p = (<$> p) . (*)
divide p = (<$> p) . flip div
