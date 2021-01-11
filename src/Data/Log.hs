{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Log where

import Data.String

newtype Log s v = Log ([[Either s v]] -> [[Either s v]])
instance Semigroup (Log s v) where Log l <> Log r = Log $ l . r
instance Monoid (Log s v) where mempty = Log id
instance IsString s => IsString (Log s v) where
	fromString = Log . (++) . ((: []) . Left . fromString <$>) . lines

-- (..) :: Log s v -> Log s v -> Log s v
-- Log l .. Log r = Log \s ->
