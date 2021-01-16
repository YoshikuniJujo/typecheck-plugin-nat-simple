{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.CanDerive (
	-- * CAN DERIVE
	canDerive,
	-- * GIVEN
	Givens, givens,
	-- * WANTED
	Wanted, wanted,
	-- * OTHERS
	Result(..), isOk
	) where

import Prelude hiding (unwords, log)

import Control.Arrow (second)
import Control.Monad ((<=<))
import Control.Monad.Try (Try, throw, Set, tell, cons)
import Data.Either (partitionEithers)
import Data.List (unfoldr, (\\), nub, partition, sort)
import Data.Map.Strict (empty)
import Data.Bool (bool)
import Data.String (IsString)
import Data.Log (Log, (.+.), unwords, log)
import Data.Derivation.Constraint (
	Constraint,
	vars, has, isDerivFrom, positives, selfContained, eliminate )
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

canDerive :: (IsString s, Set (Log s v) (Log s v), Ord v) =>
	Givens v -> Wanted v -> Try e (Log s v) (Result [Wanted1 v])
canDerive g = allM (canDerive1 g) . unWanted

allM :: Monad m => (a -> m (Result b)) -> [a] -> m (Result [b])
p `allM` xs = resultAnd <$> p `mapM` xs

resultAnd :: [Result a] -> Result [a]
resultAnd [] = Ok
resultAnd (Ok : rs) = resultAnd rs
resultAnd (Err x : rs) = case resultAnd rs of
	Ok -> Err [x]
	Err xs -> Err $ x : xs

data Result a = Ok | Err a deriving Show

isOk :: Result a -> Bool
isOk Ok = True
isOk (Err _) = False

canDerive1 :: forall s v e . (IsString s, Set (Log s v) (Log s v), Ord v) => Givens v -> Wanted1 v -> Try e (Log s v) (Result (Wanted1 v))
canDerive1 g w = do
	if s then tell $ "canDerive1: " .+. (log w :: Log s v) .+. " is self-contained" else
		tell $ "canDerive1: " .+. (log w :: Log s v) .+. " can" .+. (if d then "" else "not" ) .+. " be derive from"
	pure if s || d then Ok else Err w
	where
	s = selfContained w
	d = any (w `isDerivFrom`) (unGivens . foldr rmVar g $ gvnVars g \\ vars w)

---------------------------------------------------------------------------
-- GIVEN
---------------------------------------------------------------------------

-- NEWTYPE GIVEN AND CONSTRUCTOR

newtype Givens v = Givens { unGivens :: [Constraint v] } deriving Show

givens :: forall s v . (IsString s, Ord v) =>
	[Exp v 'Boolean] -> Try (Log s v) (Log s v) (Givens v)
givens es = do
	tell $ "givens: " .+. unwords (log <$> es :: [Log s v])
	Givens . nub . sort . ((++) <$> id <*> (positives <$>)) . concat
		<$> (uncurry cons <=< constraint (varBool es)) `mapM` es

-- GIVEN VARIABLES

gvnVars :: Ord v => Givens v -> [Maybe v]
gvnVars = nub . sort . concat . (vars <$>) . unGivens

-- REMOVE VARIABLE

rmVar :: Ord v => Maybe v -> Givens v -> Givens v
rmVar v (Givens gs) = Givens . sort . concat . uncurry (:)
	. second (unfoldUntil null (rvStep v)) $ partition (not . (`has` v)) gs

rvStep :: Ord v => Maybe v -> [Constraint v] -> ([Constraint v], [Constraint v])
rvStep _ [] = ([], [])
rvStep v (c : cs) = partitionEithers $ rmVar1 v c <$> cs

rmVar1 :: Ord v => Maybe v -> Constraint v ->
	Constraint v -> Either (Constraint v) (Constraint v)
rmVar1 v c0 c = maybe (Right c) Left $ eliminate v c0 c

unfoldUntil :: (s -> Bool) -> (s -> (r, s)) -> s -> [r]
unfoldUntil p f = unfoldr $ flip bool Nothing <$> Just . f <*> p

---------------------------------------------------------------------------
-- WANTED
---------------------------------------------------------------------------

newtype Wanted v = Wanted { unWanted :: [Wanted1 v] } deriving Show

type Wanted1 v = Constraint v

wanted :: forall s v . (IsString s, Ord v) =>
	Exp v 'Boolean -> Try (Log s v) (Log s v) (Wanted v)
wanted x = do
	tell $ "wanted: " .+. (log x :: Log s v)
	constraint empty x >>= \(e, s) -> either throw (pure . Wanted . (: s)) e
