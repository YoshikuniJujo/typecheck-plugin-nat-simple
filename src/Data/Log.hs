{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Log where

import Data.String

newtype Log s v = Log ([[Either s v]] -> [[Either s v]])
instance Semigroup (Log s v) where Log l <> Log r = Log $ l . r
instance Monoid (Log s v) where mempty = Log id
instance IsString s => IsString (Log s v) where
	fromString = Log . (++) . ((: []) . Left . fromString <$>) . lines

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
messageLog1 = concatMap $ either message show

logVar :: v -> Log s v
logVar v = Log ([Right v] :)
