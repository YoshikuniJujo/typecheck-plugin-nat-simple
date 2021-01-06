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

-- | > plugin = pluginWith \gs _ w ->
--   >	log "givens" gs
--   >	log "wanted" w
--   >	let	(gs', lg) = decodeAll @SDocStr gs
--   >	log "decodeAll log" lg
--   >	log "givens" gs'
--   >	w' <- decode w
--   >	canDerive (given gs') <$> wanted w'

plugin :: Plugin
plugin = pluginWith "Plugin.TypeCheck.Nat.Simple" \gs _ w -> do
	log "givens" gs
	log "wanted" w
	let	(gs', lg) = decodeAll @SDocStr gs
	log "decodeAll log" lg
	log "givens" gs'
	w' <- decode w
	log "wanted" w'
	canDerive <$> given gs' <*> wanted w'
