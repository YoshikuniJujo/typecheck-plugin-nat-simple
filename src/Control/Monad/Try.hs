{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Try (
	-- * DATA TRY
	Try,
	-- * RUN TRY
	runTry, gatherSuccess,
	-- * THROW AND CATCH ERROR
	throw, catch, rights,
	-- * WRITE AND GET LOG
	Set, tell, log, partial,
	-- * LOG STRING
	SDocStr, Message, message ) where

import Prelude hiding (log)

import Outputable (Outputable(..), ppr, SDoc, (<+>), ($$), text)
import Control.Applicative (Alternative(..))
import Control.Arrow ((***))
import Control.Monad (MonadPlus)
import Data.Maybe (catMaybes)
import Data.String (IsString(..))

import qualified Outputable as O (empty)

---------------------------------------------------------------------------

-- * DATA TRY
--	+ DATA
--	+ INSTANCE
-- * RUN TRY
-- * THROW AND CATCH ERROR
-- * WRITE AND GET LOG
-- * LOG STRING
--	+ MESSAGE
--	+ SDOC STRING

---------------------------------------------------------------------------
-- DATA TRY
---------------------------------------------------------------------------

-- DATA

data Try e w a = Try (Either e a) w deriving Show

-- INSTANCE

instance Functor (Try e w) where
	_ `fmap` Try (Left e) lg = Try (Left e) lg
	f `fmap` Try (Right x) lg = Try (Right $ f x) lg

instance Monoid w => Applicative (Try e w) where
	pure = (`Try` mempty) . Right
	Try (Left e) lg <*> _ = Try (Left e) lg
	Try (Right f) lg <*> mx =
		let Try (Right y) lg' = f <$> mx in Try (Right y) (lg <> lg')

instance (Monoid e, Monoid w) => Alternative (Try e w) where
	empty = Try (Left mempty) mempty
	Try (Right x) lg <|> _ = Try (Right x) lg
	Try (Left _) lg <|> Try rtn lg' = Try rtn (lg <> lg')

instance Monoid w => Monad (Try e w) where
	Try (Left e) lg >>= _ = Try (Left e) lg
	Try (Right x) lg >>= f = let Try rtn lg' = f x in Try rtn (lg <> lg')

instance (Monoid e, Monoid w) => MonadPlus (Try e w)

---------------------------------------------------------------------------
-- RUN TRY
---------------------------------------------------------------------------

runTry :: Try e w a -> (Either e a, w)
runTry (Try rtn lg) = (rtn, lg)

resume' :: Monoid s => Try s s a -> (Maybe a, s)
resume' (Try (Left e) lg) = (Nothing, lg <> e)
resume' (Try (Right x) lg) = (Just x, lg)

gatherSuccess, rights' :: Monoid w => [Try w w a] -> ([a], w)
gatherSuccess = rights'
rights' = (catMaybes *** mconcat) . unzip . map resume'

---------------------------------------------------------------------------
-- THROW AND CATCH ERROR
---------------------------------------------------------------------------

throw :: Monoid w => e -> Try e w a
throw e = Try (Left e) mempty

catch :: Semigroup w => Try e w a -> (e -> Try e w a) -> Try e w a
Try (Left e) lg `catch` f = let Try rtn lg' = f e in Try rtn (lg <> lg')
t@(Try (Right _) _) `catch` _ = t

resume :: (Set w w, Monoid w) => Try w w a -> Try w w (Maybe a)
resume = (`catch` \e -> tell e >> pure Nothing) . (Just <$>)

rights :: (Set w w, Monoid w) => [Try w w a] -> Try w w [a]
rights ts = catMaybes <$> sequence (resume <$> ts)

---------------------------------------------------------------------------
-- WRITE AND GET LOG
---------------------------------------------------------------------------

class Set x xs where set :: x -> xs

instance Set x x where set = id

instance Monoid xs => Set x (x, xs) where set x = (x, mempty)

instance {-# OVERLAPPABLE #-} (Monoid y, Set x xs) => Set x (y, xs) where
	set x = (mempty, set x)

tell :: Set w ws => w -> Try e ws ()
tell = Try (Right ()) . set

log :: (Set SDocStr ws, Outputable o) => String -> o -> Try e ws ()
log ttl o = tell . SDocStr $ text (ttl ++ ":") <+> ppr o

partial :: Try e (w, ws) a -> Try e ws (Either e a, w)
partial (Try mx (w, ws)) = Try (Right (mx, w)) ws

---------------------------------------------------------------------------
-- LOG STRING
---------------------------------------------------------------------------

-- MESSAGE

newtype Message = Message ([String] -> [String])
instance Semigroup Message where Message l <> Message r = Message $ l . r
instance Monoid Message where mempty = Message id
instance IsString Message where fromString = Message . (++) . lines
instance Show Message where show (Message m) = show . unlines $ m []

message :: Message -> String
message (Message ls) = unlines $ ls []

-- SDOC STRING

data SDocStr = SDocStrEmpty | SDocStr SDoc

instance Semigroup SDocStr where
	SDocStrEmpty <> r = r
	l <> SDocStrEmpty = l
	SDocStr l <> SDocStr r = SDocStr $ l $$ r

instance Monoid SDocStr where mempty = SDocStrEmpty
instance IsString SDocStr where fromString = SDocStr . text
instance Outputable SDocStr where
	ppr SDocStrEmpty = O.empty
	ppr (SDocStr s) = s
