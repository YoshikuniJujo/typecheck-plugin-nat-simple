{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.CanDerive (
	-- * CAN DERIVE
	canDerive,
	-- * GIVEN
	Given, given,
	-- * WANTED
	Wanted, wanted ) where

-- import Control.Monad.Trans.Except
import Data.Either
import Data.List ((\\), nub, partition, sort, unfoldr)
import Data.Map.Strict (empty)
import Data.Bool

import Data.Derivation.Constraint (
	Constraint, vars, hasVar,
	rmNegative, isDerivFrom, selfContained )
import Data.Derivation.Expression.Internal

import qualified Data.Derivation.Constraint as C

-- import Data.Except.Message
import Control.Monad.Try
import Data.String

newtype Given v = Given { unGiven :: [Constraint v] } deriving Show

given :: (Monoid s, IsString s, Set s s, Ord v) => [Exp v Bool] -> Try s s (Given v)
given es = gvn . concat <$> (mapM procGivenErr =<< constraint (varBool es) `mapM` es)
--	$ uncurry (maybe id (:)) . constraint (varBool es) <$> es

procGivenErr :: (Set s s, Monoid s) => (Either s (Constraint v), [Constraint v]) -> Try s s [Constraint v]
procGivenErr (Left e, cs) = tell e >> pure cs
procGivenErr (Right c, cs) = pure $ c : cs

gvn :: Ord v => [Constraint v] -> Given v
gvn zs = Given . nub . sort $ zs ++ (rmNegative <$> zs)

gvnVars :: Ord v => Given v -> [Maybe v]
gvnVars = nub . sort . concat . (vars <$>) . unGiven

newtype Wanted v = Wanted { unWanted :: [Wanted1 v] } deriving Show

type Wanted1 v = Constraint v

wanted :: (Monoid s, IsString e, Ord v) => Exp v Bool -> Try e s (Wanted v)
wanted = wantedGen

wantedGen :: (Monoid s, IsString e, Ord v) => Exp v Bool -> Try e s (Wanted v)
wantedGen ex = do
	(ec, cs) <- constraint empty ex
	case ec of
		Left er -> throw er
		Right c -> pure . Wanted $ c : cs

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
