{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
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
	_ `fmap` Try (Left e) w = Try (Left e) w
	f `fmap` Try (Right x) w = Try (Right $ f x) w

instance Monoid w => Applicative (Try e w) where
	pure = (`Try` mempty) . Right
	Try (Left e) w <*> _ = Try (Left e) w
	Try (Right f) w <*> mx =
		let Try (Right y) w' = f <$> mx in Try (Right y) (w <> w')

instance (Monoid e, Monoid w) => Alternative (Try e w) where
	empty = Try (Left mempty) mempty
	Try (Right x) w <|> _ = Try (Right x) w
	Try (Left _) w <|> Try rtn w' = Try rtn (w <> w')

instance Monoid w => Monad (Try e w) where
	Try (Left e) w >>= _ = Try (Left e) w
	Try (Right x) w >>= f = let Try rtn w' = f x in Try rtn (w <> w')

instance (Monoid e, Monoid w) => MonadPlus (Try e w)

---------------------------------------------------------------------------
-- RUN TRY
---------------------------------------------------------------------------

runTry :: Try e w a -> (Either e a, w)
runTry (Try rtn w) = (rtn, w)

gatherSuccess :: Monoid w => [Try w w a] -> ([a], w)
gatherSuccess = (catMaybes *** mconcat) . unzip . map \case
	(Try (Left e) w) -> (Nothing, w <> e)
	(Try (Right x) w) -> (Just x, w)

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
