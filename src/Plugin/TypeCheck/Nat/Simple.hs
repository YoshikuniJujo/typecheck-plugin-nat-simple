{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple (
	-- * PLUGIN
	plugin ) where

import GhcPlugins

import Plugin.TypeCheck.Nat.Simple.Decode

import Data.Derivation.CanDerive

import Plugin.TypeCheck.Nat.Simple.PluginWith

import Control.Monad.Try

-- | > plugin = pluginWith \gs _ w ->
--   >	tell "foobar"
--   >	canDerive (given $ decodeAll gs) <$> (wanted =<< decode w)

plugin :: Plugin
plugin = pluginWith \gs _ w -> do
	tell "foobar"
	canDerive (given $ decodeAll gs) <$> (wanted =<< decode w)
