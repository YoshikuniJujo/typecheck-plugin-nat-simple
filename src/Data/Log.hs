{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Log where

newtype Log s v = Log ([[Either s v]] -> [[Either s v]])
instance Semigroup (Log s v) where Log l <> Log r = Log $ l . r
instance Monoid (Log s v) where mempty = Log id
