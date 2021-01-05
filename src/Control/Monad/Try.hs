{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Try where

import Control.Applicative
import Control.Arrow
import Outputable (Outputable(..), SDoc, ($$), text)
import Data.Maybe
import Data.String

import qualified Outputable as O

data Try s a = Try (Either s a) s deriving Show

runTry :: Try s a -> (Either s a, s)
runTry (Try rtn lg) = (rtn, lg)

newtype Message = Message ([String] -> [String])
instance Semigroup Message where Message l <> Message r = Message $ l . r
instance Monoid Message where mempty = Message id
instance IsString Message where fromString = Message . (:)

message :: Message -> String
message (Message ls) = unlines $ ls []

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

instance Functor (Try s) where
	_ `fmap` Try (Left e) lg = Try (Left e) lg
	f `fmap` Try (Right x) lg = Try (Right $ f x) lg

instance Monoid s => Applicative (Try s) where
	pure x = Try (Right x) mempty
	Try (Left e) lg <*> _ = Try (Left e) lg
	Try (Right f) lg <*> mx =
		let Try (Right y) lg' = f <$> mx in Try (Right y) (lg <> lg')

instance Monoid s => Monad (Try s) where
	Try (Left e) lg >>= _ = Try (Left e) lg
	Try (Right x) lg >>= f =
		let Try rtn lg' = f x in Try rtn (lg <> lg')

instance Monoid s => Alternative (Try s) where
	empty = Try (Left mempty) mempty
	Try (Right x) lg <|> _ = Try (Right x) lg
	Try (Left _) lg <|> Try rtn lg' = Try rtn (lg <> lg')

throw :: Monoid s => s -> Try s a
throw e = Try (Left e) mempty

catch :: Semigroup s => Try s a -> (s -> Try s a) -> Try s a
Try (Left e) lg `catch` f = let Try rtn lg' = f e in Try rtn (lg <> lg')
t@(Try (Right _) _) `catch` _ = t

tell :: s -> Try s ()
tell = Try (Right ())

resume :: Monoid s => Try s a -> (Maybe a, s)
resume (Try (Left e) lg) = (Nothing, lg <> e)
resume (Try (Right x) lg) = (Just x, lg)

rights :: Monoid s => [Try s a] -> ([a], s)
rights = (catMaybes *** mconcat) . unzip . map resume
