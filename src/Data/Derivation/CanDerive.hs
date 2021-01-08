{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.CanDerive (
	-- * CAN DERIVE
	canDerive,
	-- * GIVEN
	Given, given,
	-- * WANTED
	Wanted, wanted ) where

import Control.Monad ((<=<))
import Control.Monad.Try (Try, throw, Set, cons)
import Data.Either (partitionEithers)
import Data.List (unfoldr, (\\), nub, partition, sort)
import Data.Map.Strict (empty)
import Data.Bool (bool)
import Data.String (IsString)

import Data.Derivation.Constraint (
	Constraint,
	vars, hasVar, isDerivFrom, positives, selfContained, eliminate )
import Data.Derivation.Expression.Internal (
	Exp, ExpType(..), constraint, varBool )

---------------------------------------------------------------------------

-- * CAN DERIVE
-- * GIVEN
--	+ NEWTYPE GIVEN AND CONSTRUCTOR
--	+ GIVEN VARIABLES
--	+ REMOVE VARIABLE
-- * WANTED

---------------------------------------------------------------------------
-- CAN DERIVE
---------------------------------------------------------------------------

canDerive :: Ord v => Given v -> Wanted v -> Bool
canDerive g = all (canDerive1 g) . unWanted

canDerive1 :: Ord v => Given v -> Wanted1 v -> Bool
canDerive1 g w = selfContained w ||
	any (w `isDerivFrom`) (unGiven . foldl rmVar g $ gvnVars g \\ vars w)

---------------------------------------------------------------------------
-- GIVEN
---------------------------------------------------------------------------

-- NEWTYPE GIVEN AND CONSTRUCTOR

newtype Given v = Given { unGiven :: [Constraint v] } deriving Show

given :: (Monoid s, IsString s, Set s s, Ord v) =>
	[Exp v 'Boolean] -> Try s s (Given v)
given es = Given . nub . sort . ((++) <$> id <*> (positives <$>)) . concat
	<$> (uncurry cons <=< constraint (varBool es)) `mapM` es

-- GIVEN VARIABLES

gvnVars :: Ord v => Given v -> [Maybe v]
gvnVars = nub . sort . concat . (vars <$>) . unGiven

-- REMOVE VARIABLE

rmVar :: Ord v => Given v -> Maybe v -> Given v
rmVar (Given g) v =
	Given . sort $ r ++ concat (unfoldUntil null (`rvStep` v) g')
	where (g', r) = partition (`hasVar` v) g

rvStep :: Ord v => [Constraint v] -> Maybe v -> ([Constraint v], [Constraint v])
rvStep [] _ = ([], [])
rvStep (c : cs) v = partitionEithers $ flip (rmVar1 c) v <$> cs

rmVar1 :: Ord v => Constraint v ->
	Constraint v -> Maybe v -> Either (Constraint v) (Constraint v)
rmVar1 c0 c v = maybe (Right c) Left $ eliminate c0 c v

unfoldUntil :: (s -> Bool) -> (s -> (r, s)) -> s -> [r]
unfoldUntil p f = unfoldr \s -> bool (Just $ f s) Nothing (p s)

---------------------------------------------------------------------------
-- WANTED
---------------------------------------------------------------------------

newtype Wanted v = Wanted { unWanted :: [Wanted1 v] } deriving Show

type Wanted1 v = Constraint v

wanted :: (Monoid s, IsString e, Ord v) => Exp v 'Boolean -> Try e s (Wanted v)
wanted = wantedGen

wantedGen :: (Monoid s, IsString e, Ord v) => Exp v 'Boolean -> Try e s (Wanted v)
wantedGen ex = do
	(ec, cs) <- constraint empty ex
	case ec of
		Left er -> throw er
		Right c -> pure . Wanted $ c : cs
