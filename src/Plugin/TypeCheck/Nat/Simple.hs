{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple (
	-- * PLUGIN
	plugin ) where

import Prelude hiding (log)

import GhcPlugins (Var, Plugin, ppr)
import Control.Monad.Try (tell)
import Data.Derivation.CanDerive (canDerive, given, wanted)

import Plugin.TypeCheck.Nat.Simple.TypeCheckWith (typeCheckWith)
import Plugin.TypeCheck.Nat.Simple.Decode (decode, decodeAll)

import Data.Log

-- | > plugin = typeCheckWith @(Log SDocStr Var)  "Plugin.TypeCheck.Nat.Simple"
--   >	\gs _ w ->
--   >		tell @(Log SDocStr Var $ "givens: " .+. fromSDoc (ppr gs)
--   >		tell @(Log SDocStr Var $ "wanted: " .+. fromSDoc (ppr w)
--   >		canDerive <$> (given =<< decodeAll gs) <*> (wanted =<< decode w)

plugin :: Plugin
plugin = typeCheckWith @(Log SDocStr Var) "Plugin.TypeCheck.Nat.Simple"
	\gs _ w -> do
		tell @(Log SDocStr Var) $ "givens: " .+. fromSDoc (ppr gs)
		tell @(Log SDocStr Var) $ "wanted: " .+. fromSDoc (ppr w)
		canDerive <$> (given =<< decodeAll gs) <*> (wanted =<< decode w)
