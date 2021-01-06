{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Parse (Parse, parse, unparse, (>>!)) where

import Control.Applicative
import Control.Arrow
import Control.Monad
-- import Control.Monad.State (StateT(..), runStateT, (>=>))

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
	f `fmap` StateT k = StateT \s -> (f `first`) <$> k s

instance Monad m => Applicative (StateT s m) where
	pure x = StateT \s -> pure (x, s)
	StateT kf <*> mx = StateT \s -> do
		(f, s') <- kf s
		(f <$> mx) `runStateT` s'

instance Monad m => Monad (StateT s m) where
	StateT k >>= f = StateT \s -> do
		(x, s') <- k s
		f x `runStateT` s'

instance (Functor m, MonadPlus m) => Alternative (StateT s m) where
	empty = StateT \_ -> mzero
	StateT m <|> StateT n = StateT \s -> m s `mplus` n s

type Parse s = StateT s Maybe

parse :: (s -> Maybe (a, s)) -> Parse s a
parse = StateT

unparse :: Parse s a -> s -> Maybe (a, s)
unparse = runStateT

(>>!) :: Parse s a -> Parse s b -> Parse s a
p >>! nla = parse $ unparse p >=> \r@(_, s) ->
	maybe (pure r) (const empty) $ unparse nla s
