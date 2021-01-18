{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple (
	-- * PLUGIN
	plugin ) where

import Prelude hiding (log)

import GhcPlugins (Var, Plugin, ppr)
import Control.Monad.Try (tell)
import Data.Log (Log, (.+.), fromSDoc, SDocStr)
import Data.Derivation.CanDerive (canDerive, givens, wanted, isOk)
import Plugin.TypeCheck.Nat.Simple.TypeCheckWith (typeCheckWith)
import Plugin.TypeCheck.Nat.Simple.Decode (decode, decodeAll)

import Data.Derivation.Expression.Internal

type L = Log SDocStr Var

-- | > type L = Log SDocStr Var
--   >
--   > plugin :: Plugin
--   > plugin = typeCheckWith @L "Plugin.TypeCheck.Nat.Simple" \gs _ w ->
--   >	tell @L $ "givens: " .+. fromSDoc (ppr gs)
--   >	tell @L $ "wanted: " .+. fromSDoc (ppr w)
--   >	canDerive <$> (givens =<< decodeAll gs) <*> (wanted =<< decode w)

plugin :: Plugin
plugin = typeCheckWith @L "Plugin.TypeCheck.Nat.Simple" \gs _ w -> do
	tell @L $ "givens: " .+. fromSDoc (ppr gs)
	tell @L $ "wanted: " .+. fromSDoc (ppr w)
	(map constraintToExp <$>) <$> (uncurry canDerive =<< (,) <$> (givens =<< decodeAll gs) <*> (wanted =<< decode w))
