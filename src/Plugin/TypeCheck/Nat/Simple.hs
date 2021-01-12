{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple (
	-- * PLUGIN
	plugin ) where

import Prelude hiding (log)

import GhcPlugins (Var, Plugin, ppr)
import Control.Monad.Try (tell)
import Control.Monad.Try(Try)
import Data.Derivation.CanDerive (canDerive, given, wanted)

import Plugin.TypeCheck.Nat.Simple.TypeCheckWith (typeCheckWith)
import Plugin.TypeCheck.Nat.Simple.Decode (decode, decodeAll)

import Data.Log

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
	pure () :: Try (Log SDocStr Var) (Log SDocStr Var) ()
	tell $ ("givens: " :: Log SDocStr Var) .+. fromSDoc (ppr gs)
	tell $ ("wanted: " :: Log SDocStr Var) .+. fromSDoc (ppr w)
	if null gs' then pure () else
		tell $ ("givens: " :: Log SDocStr Var) .+. foldr1 ((\l r -> (l .+. (" " :: Log SDocStr Var) .+. r))) (log <$> gs')
	tell $ ("wanted: " :: Log SDocStr Var) .+. log w'
--	log "givens" gs
--	log "wanted" w
--	log "givens" gs'
--	log "wanted" w'
	canDerive <$> given gs' <*> wanted w'
