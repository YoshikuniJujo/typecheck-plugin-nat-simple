{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.StateT (StateT(..)) where

import Control.Applicative (Alternative(..))
import Control.Arrow (first)
import Control.Monad (MonadPlus)

---------------------------------------------------------------------------

-- * NEWTYPE STATE T
-- * INSTANCE
--	+ FUNCTOR
--	+ APPLICATIVE AND ALTERNATIVE
--	+ MONAD AND MONAD PLUS

---------------------------------------------------------------------------
-- NEWTYPE STATE T
---------------------------------------------------------------------------

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

---------------------------------------------------------------------------
-- INSTANCE
---------------------------------------------------------------------------

-- FUNCTOR

instance Functor m => Functor (StateT s m) where
	f `fmap` StateT k = StateT \s -> (f `first`) <$> k s

-- APPLICATIVE AND ALTERNATIVE

instance Monad m => Applicative (StateT s m) where
	pure x = StateT $ pure . (x ,)
	StateT kf <*> mx =
		StateT \s -> kf s >>= \(f, s') -> (f <$> mx) `runStateT` s'

instance MonadPlus m => Alternative (StateT s m) where
	empty = StateT \_ -> empty
	StateT k <|> StateT l = StateT $ (<|>) <$> k <*> l

-- MONAD AND MONAD PLUS

instance Monad m => Monad (StateT s m) where
	StateT k >>= f = StateT \s -> k s >>= \(x, s') -> f x `runStateT` s'

instance MonadPlus m => MonadPlus (StateT s m)
