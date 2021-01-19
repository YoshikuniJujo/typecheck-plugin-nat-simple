{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.CanDerive (
	-- * CAN DERIVE
	canDerive,
	-- * GIVENS
	Givens, givens,
	-- * WANTED
	Wanted, wanted ) where

import Prelude hiding (unwords, log)

import Control.Arrow (second)
import Control.Monad ((<=<))
import Control.Monad.Try (Try, throw, Set, tell, cons)
import Data.Map.Strict (empty)
import Data.Either (partitionEithers)
import Data.List (unfoldr, (\\), nub, partition, sort)
import Data.Bool (bool)
import Data.String (IsString)
import Data.Log (Log, (.+.), intersperse, unwords, log, Loggable(..))
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

canDerive :: (IsString s, Ord v) => Givens v -> Wanted v -> Try e (Log s v) Bool
canDerive g = allM (canDerive1 g) . unWanted

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
p `allM` xs = and <$> p `mapM` xs

canDerive1 :: forall s v e . (IsString s, Set (Log s v) (Log s v), Ord v) => Givens v -> Wanted1 v -> Try e (Log s v) Bool
canDerive1 g w = do
	if s then tell $ "canDerive1: " .+. (log w :: Log s v) .+. " is self-contained" else
		tell $ "canDerive1: " .+. (log w :: Log s v) .+. " can" .+. (if d then "" else "not" ) .+. " be derive from"
	pure $ s || d
	where
	s = selfContained w
	d = any (w `isDerivFrom`) (unGivens . foldr rmVar g $ gvnVars g \\ vars w)

---------------------------------------------------------------------------
-- GIVEN
---------------------------------------------------------------------------

-- NEWTYPE GIVEN AND CONSTRUCTOR

newtype Givens v = Givens { unGivens :: [Constraint v] } deriving Show

instance IsString s => Loggable s v (Givens v) where
	log (Givens cs) = "(Givens [" .+. intersperse ", " (log <$> cs) .+. "])"

givens :: forall s v . (IsString s, Ord v) =>
	[Exp v 'Boolean] -> Try (Log s v) (Log s v) (Givens v)
givens es = do
	tell $ "givens :: [Exp v 'Boolean]: " .+. unwords (log <$> es :: [Log s v])
	gs' <- Givens . nub . sort . ((++) <$> id <*> (positives <$>)) . concat
		<$> (uncurry cons <=< constraint (varBool es)) `mapM` es
	tell $ "givens :: Givens v: " .+. (log gs' :: Log s v)
	pure gs'

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

instance IsString s => Loggable s v (Wanted v) where
	log (Wanted cs) = "(Wanted [" .+. intersperse ", " (log <$> cs) .+. "])"

type Wanted1 v = Constraint v

wanted :: forall s v . (IsString s, Ord v) =>
	Exp v 'Boolean -> Try (Log s v) (Log s v) (Wanted v)
wanted x = do
	tell $ "wanted :: Exp v 'Boolean: " .+. (log x :: Log s v)
	(e, s) <- constraint empty x
	w' <- either throw (pure . Wanted . (: s)) e
	tell $ "wanted :: Wanted v" .+. (log w' :: Log s v)
	pure w'
