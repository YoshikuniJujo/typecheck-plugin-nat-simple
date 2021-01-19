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
import Control.Monad.Try (Try, throw, tell, cons)
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
-- * GIVENS
--	+ NEWTYPE GIVENS AND CONSTRUCTOR
--	+ GIVENS VARIABLES
--	+ REMOVE VARIABLE
-- * WANTED

---------------------------------------------------------------------------
-- CAN DERIVE
---------------------------------------------------------------------------

canDerive :: (IsString s, Ord v) => Givens v -> Wanted v -> Try e (Log s v) Bool
canDerive g = (canDerive1 g `allM`) . unWanted

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
p `allM` xs = and <$> p `mapM` xs

canDerive1 :: forall s v e .
	(IsString s, Ord v) => Givens v -> Wanted1 v -> Try e (Log s v) Bool
canDerive1 g w = (s || d) <$ if s
	then tell $ ttl .+. lw .+. " is self-contained"
	else tell $ ttl .+. lw .+. " can" .+. nt .+. " be derived from"
	where
	s = selfContained w
	d = any (w `isDerivFrom`) . unGivens . foldr rmVar g $ gVars g \\ vars w
	ttl = "canDerive1: "; lw = log w :: Log s v; nt = bool "not" "" d

---------------------------------------------------------------------------
-- GIVENS
---------------------------------------------------------------------------

-- NEWTYPE GIVENS AND CONSTRUCTOR

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

-- GIVENS VARIABLES

gVars :: Ord v => Givens v -> [Maybe v]
gVars = nub . sort . concat . (vars <$>) . unGivens

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
