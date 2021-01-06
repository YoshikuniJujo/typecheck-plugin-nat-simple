{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Expression.Internal (
	Exp(..), Number, constraint, varBool ) where

import Prelude hiding ((<>))

import Outputable (Outputable(..), (<>), (<+>), text)
import Control.Arrow (first, second)
import Data.Map.Strict (Map, (!?), empty, singleton, insert)
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Derivation.Constraint (
	Constraint, equal, greatEqualThan, greatThan, Polynomial, (.+), (.-) )

import Control.Monad.Try
import Data.String

---------------------------------------------------------------------------

-- * DATA EXP
-- * CONSTRAINT
--	+ CONSTRAINT
--	+ PROCESS EQUATION
-- * POLYNOMIAL
-- * MAP FROM VARIABLE TO BOOL

---------------------------------------------------------------------------
-- DATA EXP
---------------------------------------------------------------------------

data Exp v t where
	Bool :: Bool -> Exp v Bool
	Var :: v -> Exp v t
	Const :: Integer -> Exp v Number
	(:==) :: Exp v t -> Exp v t -> Exp v Bool
	(:<=) :: Exp v Number -> Exp v Number -> Exp v Bool
	(:+) :: Exp v Number -> Exp v Number -> Exp v Number
	(:-) :: Exp v Number -> Exp v Number -> Exp v Number

data Number

deriving instance Show v => Show (Exp v t)

instance Outputable v => Outputable (Exp v t) where
	ppr (Bool b) = text "(Bool" <+> ppr b <> text ")"
	ppr (Var v) = text "(Var" <+> ppr v <> text ")"
	ppr (Const n) = text "(Const" <+> ppr n <> text ")"
	ppr (l :== r) = text "(" <> ppr l <+> text ":==" <+> ppr r <> text ")"
	ppr (l :<= r) = text "(" <> ppr l <+> text ":<=" <+> ppr r <> text ")"
	ppr (l :+ r) = text "(" <> ppr l <+> text ":+" <+> ppr r <> text ")"
	ppr (l :- r) = text "(" <> ppr l <+> text ":-" <+> ppr r <> text ")"

---------------------------------------------------------------------------
-- CONSTRAINT
---------------------------------------------------------------------------

-- CONSTRAINT

constraint :: (Monoid s, IsString e, Ord v) =>
	VarBool v -> Exp v Bool -> Try e s (Either e (Constraint v), [Constraint v])
constraint vb e = partial $ procEq vb e True

-- PROCCESS EQUATION

procEq :: (Monoid s, IsString e, Ord v) => VarBool v ->
	Exp v Bool -> Bool -> Try e ([Constraint v], s) (Constraint v)
procEq _ (Bool _) _ = throw "procEq: bad"; procEq _ (Var _) _ = throw "procEq: bad"
procEq _ (l :<= r) False = greatThan <$> poly l <*> poly r
procEq _ (l :<= r) True = greatEqualThan <$> poly r <*> poly l
procEq vb (l :== Bool r) b = procEq vb l (r == b)
procEq vb (Bool l :== r) b = procEq vb r (l == b)
procEq vb (l :== Var r) b | Just br <- vb !? r = case l of
	_ :== _ -> procEq vb l (br == b); _ :<= _ -> procEq vb l (br == b)
	_ -> throw "procEq: bad"
procEq vb (Var l :== r) b | Just bl <- vb !? l = case r of
	_ :== _ -> procEq vb r (bl == b); _ :<= _ -> procEq vb r (bl == b)
	_ -> throw "procEq: bad"
procEq _ (l :== r) True = case (l, r) of
	(Const _, _) -> equal <$> poly l <*> poly r
	(_ :+ _, _) -> equal <$> poly l <*> poly r
	(_ :- _, _) -> equal <$> poly l <*> poly r
	(_, Const _) -> equal <$> poly l <*> poly r
	(_, _ :+ _) -> equal <$> poly l <*> poly r
	(_, _ :- _) -> equal <$> poly l <*> poly r
	(Var v, Var w) -> equal <$> poly (Var v) <*> poly (Var w)
	_ -> throw "procEq: bad"
procEq _ (_ :== _) False = throw "procEq: bad"

---------------------------------------------------------------------------
-- POLYNOMIAL
---------------------------------------------------------------------------

poly :: (Monoid s, Ord v) => Exp v Number -> Try e ([Constraint v], s) (Polynomial v)
poly (Const 0) = pure empty
poly (Const n) = pure $ singleton Nothing n
poly (Var v) = p <$ tell [p `greatEqualThan` empty]
	where p = singleton (Just v) 1
poly (l :+ r) = (.+) <$> poly l <*> poly r
poly (l :- r) = (,) <$> poly l <*> poly r >>= \(pl, pr) ->
	pl .- pr <$ tell [pl `greatEqualThan` pr]

---------------------------------------------------------------------------
-- MAP FROM VARIABLES TO BOOL
---------------------------------------------------------------------------

type VarBool v = Map v Bool

varBool :: Ord v => [Exp v Bool] -> VarBool v
varBool = snd . untilFixed (uncurry vbStep) . vbInit

vbInit :: Ord v => [Exp v Bool] -> ([(v, v)], VarBool v)
vbInit [] = ([], empty)
vbInit (Var l :== Var r : es) = ((l, r) :) `first` vbInit es
vbInit (Var l :== Bool r : es) = insert l r `second` vbInit es
vbInit (Bool l :== Var r : es) = insert r l `second` vbInit es
vbInit (_ : es) = vbInit es

vbStep :: Ord v => [(v, v)] -> VarBool v -> ([(v, v)], VarBool v)
vbStep [] vb = ([], vb)
vbStep ((l, r) : vs) vb = case (vb !? l, vb !? r) of
	(Just bl, _) -> vbStep vs $ insert r bl vb
	(Nothing, Just br) -> vbStep vs $ insert l br vb
	(Nothing, Nothing) -> ((l, r) :) `first` vbStep vs vb

untilFixed :: Eq a => (a -> a) -> a -> a
untilFixed f x = fst . fromJust . find (uncurry (==)) $ zip xs (tail xs)
	where xs = iterate f x
