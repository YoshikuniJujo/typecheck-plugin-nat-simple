{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Log where

import Outputable (Outputable, SDoc, empty, ppr, ($$), text)
import Data.String

import qualified Outputable as O

newtype Log s v = Log ([[Either s v]] -> [[Either s v]])
instance Semigroup (Log s v) where Log l <> Log r = Log $ l . r
instance Monoid (Log s v) where mempty = Log id
instance IsString s => IsString (Log s v) where
	fromString = Log . (++) . ((: []) . Left . fromString <$>) . lines
instance IsSDoc s => IsSDoc (Log s v) where
	fromSDoc = Log . (:) . (: []) . Left . fromSDoc

infixr 7 .+.

(.+.) :: Log s v -> Log s v -> Log s v
Log l .+. Log r = Log $ (l [] %) . r
	where
	(%) :: [[a]] -> [[a]] -> [[a]]
	[] % yss = yss
	[xs] % (ys : yss) = (xs ++ ys) : yss
	(xs : xss) % yss = xs : (xss % yss)

class Message s where
	message :: s -> String
	messageList :: [s] -> String
	messageList = unlines . (message <$>)

instance Message s => Message [s] where message = messageList

instance Message Char where
	message = (: [])
	messageList = id

instance (Show s, Show v) => Show (Log s v) where
	show (Log k) = "Log (" ++ show (k []) ++ " ++)"

instance (Message s, Show v) => Message (Log s v) where
	message (Log k) = unlines $ messageLog1 <$> k []

messageLog1 :: (Message s, Show v) => [Either s v] -> String
messageLog1 = concatMap (either message show)

logVar :: v -> Log s v
logVar v = Log ([Right v] :)

instance (Outputable s, Outputable v) => Outputable (Log s v) where
	ppr (Log k) = foldr (O.$$) empty $ pprLog1 <$> k []

pprLog1 :: (Outputable s, Outputable v) => [Either s v] -> SDoc
pprLog1 = foldr (O.<>) empty . (either ppr ppr <$>)

class Loggable s v a where log :: a -> Log s v

class IsSDoc s where fromSDoc :: SDoc -> s

-- SDOC STRING

data SDocStr = SDocStrEmpty | SDocStr SDoc

instance Semigroup SDocStr where
	SDocStrEmpty <> r = r; l <> SDocStrEmpty = l
	SDocStr l <> SDocStr r = SDocStr $ l $$ r

instance Monoid SDocStr where mempty = SDocStrEmpty
instance IsString SDocStr where fromString = SDocStr . text
instance Outputable SDocStr where
	ppr SDocStrEmpty = O.empty; ppr (SDocStr s) = s

instance IsSDoc SDocStr where fromSDoc = SDocStr
