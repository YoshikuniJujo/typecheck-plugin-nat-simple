{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Log (
	-- * LOG
	-- ** DATA LOG
	Log, (.+.), unwords, logVar,
	-- ** CLASS
	Loggable(..), Message(..),
	-- * SDOC
	IsSDoc(..), SDocStr ) where

import Prelude hiding (unwords, log)

import Outputable (Outputable, SDoc, empty, ppr, ($$), text)
import Data.String (IsString(..))

import qualified Outputable as O ((<>))

---------------------------------------------------------------------------

-- * LOG
--	+ NEWTYPE LOG
--	+ INSTANCE
--	+ FUNCTION
--	+ CLASS
-- * SDOC

---------------------------------------------------------------------------
-- LOG
---------------------------------------------------------------------------

-- NEWTYPE LOG

newtype Log s v = Log ([[Either s v]] -> [[Either s v]])

-- INSTANCE

instance Semigroup (Log s v) where Log l <> Log r = Log $ l . r
instance Monoid (Log s v) where mempty = Log id

instance IsString s => IsString (Log s v) where
	fromString = Log . (++) . ((: []) . Left . fromString <$>) . lines

instance IsSDoc s => IsSDoc (Log s v) where
	fromSDoc = Log . (:) . (: []) . Left . fromSDoc

instance (Show s, Show v) => Show (Log s v) where
	show (Log k) = "(Log (" ++ show (k []) ++ " ++))"

instance (Outputable s, Outputable v) => Outputable (Log s v) where
	ppr (Log k) = foldr ($$) empty $ pprLog1 <$> k []

pprLog1 :: (Outputable s, Outputable v) => [Either s v] -> SDoc
pprLog1 = foldr (O.<>) empty . (either ppr ppr <$>)

instance (Message s, Show v) => Message (Log s v) where
	message (Log k) = unlines $ messageLog1 <$> k []

messageLog1 :: (Message s, Show v) => [Either s v] -> String
messageLog1 = concatMap (either message show)

-- FUNCTION

infixr 7 .+.

(.+.) :: Log s v -> Log s v -> Log s v
Log l .+. Log r = Log $ (l [] %) . r
	where
	(%) :: [[a]] -> [[a]] -> [[a]]
	[] % yss = yss
	[xs] % (ys : yss) = (xs ++ ys) : yss
	(xs : xss) % yss = xs : (xss % yss)

logVar :: v -> Log s v
logVar v = Log ([Right v] :)

unwords :: IsString s => [Log s v] -> Log s v
unwords [] = mempty
unwords ls = foldr1 (\l r -> l .+. " " .+. r) ls

-- CLASS

class Loggable s v a where log :: a -> Log s v

class Message s where
	message :: s -> String
	messageList :: [s] -> String
	messageList = unlines . (message <$>)

instance Message s => Message [s] where message = messageList

instance Message Char where
	message = (: [])
	messageList = id

---------------------------------------------------------------------------
-- SDOC STRING
---------------------------------------------------------------------------

class IsSDoc s where fromSDoc :: SDoc -> s

data SDocStr = SDocStrEmpty | SDocStr SDoc

instance Semigroup SDocStr where
	SDocStrEmpty <> r = r; l <> SDocStrEmpty = l
	SDocStr l <> SDocStr r = SDocStr $ l $$ r

instance Monoid SDocStr where mempty = SDocStrEmpty
instance IsString SDocStr where fromString = SDocStr . text
instance Outputable SDocStr where
	ppr SDocStrEmpty = empty; ppr (SDocStr s) = s

instance IsSDoc SDocStr where fromSDoc = SDocStr
