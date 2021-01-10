{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple (
	-- * PLUGIN
	plugin ) where

import Prelude hiding (log)

import GhcPlugins (Plugin)
import Control.Monad.Try (log)
import Data.Derivation.CanDerive (canDerive, given, wanted)

import Plugin.TypeCheck.Nat.Simple.TypeCheckWith (typeCheckWith)
import Plugin.TypeCheck.Nat.Simple.Decode (decode, decodeAll)

-- | > plugin = typeCheckWith "Plugin.TypeCheck.Nat.Simple" \gs _ w ->
--   >	gs' <- decodeAll gs
--   >	w' <- decode w
--   >	log "givens" gs
--   >	log "wanted" w
--   >	log "givens" gs'
--   >	log "wanted" w'
--   >	canDerive <$> given gs' <*> wanted w'

plugin :: Plugin
plugin = typeCheckWith "Plugin.TypeCheck.Nat.Simple" \gs _ w -> do
	gs' <- decodeAll gs
	w' <- decode w
	log "givens" gs
	log "wanted" w
	log "givens" gs'
	log "wanted" w'
	canDerive <$> given gs' <*> wanted w'
