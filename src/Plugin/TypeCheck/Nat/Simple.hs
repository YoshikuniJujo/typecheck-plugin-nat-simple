{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple (
	-- * PLUGIN
	plugin ) where

import Prelude hiding (log)

import GhcPlugins

import Plugin.TypeCheck.Nat.Simple.Decode

import Data.Derivation.CanDerive

import Plugin.TypeCheck.Nat.Simple.PluginWith

import Control.Monad.Try

-- | > plugin = pluginWith "Plugin.TypeCheck.Nat.Simple" \gs _ w ->
--   >	gs' <- decodeAll gs
--   >	w' <- decode w
--   >	log "givens" gs
--   >	log "wanted" w
--   >	log "givens" gs'
--   >	log "wanted" w'
--   >	canDerive <$> given gs' <*> wanted w'

plugin :: Plugin
plugin = pluginWith "Plugin.TypeCheck.Nat.Simple" \gs _ w -> do
	gs' <- decodeAll gs
	w' <- decode w
	log "givens" gs
	log "wanted" w
	log "givens" gs'
	log "wanted" w'
	canDerive <$> given gs' <*> wanted w'
