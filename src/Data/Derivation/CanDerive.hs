{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.CanDerive (
	-- * CAN DERIVE
	canDerive,
	-- * GIVEN
	Given, given,
	-- * WANTED
	Wanted, wanted ) where

import Data.Either
import Data.List ((\\), nub, partition, sort, unfoldr)
import Data.Map.Strict (empty)
import Data.Bool

import Data.Derivation.Constraint (
	Constraint, vars, hasVar,
	rmNegative, isDerivFrom, selfContained )
import Data.Derivation.Expression.Internal

import qualified Data.Derivation.Constraint as C

newtype Given v = Given { unGiven :: [Constraint v] } deriving Show

given :: Ord v => [Exp v Bool] -> Given v
given es = gvn . concat
	$ uncurry (maybe id (:)) . constraint (varBool es) <$> es

gvn :: Ord v => [Constraint v] -> Given v
gvn zs = Given . nub . sort $ zs ++ (rmNegative <$> zs)

gvnVars :: Ord v => Given v -> [Maybe v]
gvnVars = nub . sort . concat . (vars <$>) . unGiven

newtype Wanted v = Wanted { unWanted :: [Wanted1 v] } deriving Show

type Wanted1 v = Constraint v

wanted :: Ord v => Exp v Bool -> Maybe (Wanted v)
wanted = uncurry wntd . constraint empty

wntd :: Maybe (Wanted1 v) -> [Wanted1 v] -> Maybe (Wanted v)
wntd mw ws = Wanted . (: ws) <$> mw

rmVar :: Ord v => Given v -> Maybe v -> Given v
rmVar (Given g) v =
	Given . sort $ r ++ concat (unfoldUntil null (`rvStep` v) g')
	where (g', r) = partition (`hasVar` v) g

rvStep :: Ord v => [Constraint v] -> Maybe v -> ([Constraint v], [Constraint v])
rvStep [] _ = ([], [])
rvStep (c : cs) v = partitionEithers $ flip (rmVar1 c) v <$> cs

rmVar1 :: Ord v => Constraint v ->
	Constraint v -> Maybe v -> Either (Constraint v) (Constraint v)
rmVar1 c0 c v = maybe (Right c) Left $ C.eliminate c0 c v

unfoldUntil :: (s -> Bool) -> (s -> (r, s)) -> s -> [r]
unfoldUntil p f = unfoldr \s -> bool (Just $ f s) Nothing (p s)

canDerive :: Ord v => Given v -> Wanted v -> Bool
canDerive g = all (canDerive1 g) . unWanted

canDerive1 :: Ord v => Given v -> Wanted1 v -> Bool
canDerive1 g w = selfContained w ||
	any (isDerivFrom w) (unGiven . foldl rmVar g $ gvnVars g \\ vars w)
