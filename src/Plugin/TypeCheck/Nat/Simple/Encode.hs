{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.Encode (encode) where

import GhcPlugins (Var)
import TcPluginM
import TcRnTypes
import TyCoRep
import Data.Derivation.Expression
import Plugin.TypeCheck.Nat.Simple.UnNomEq

encode :: Ct -> Exp Var 'Boolean -> TcPluginM Ct
encode _ (Bool _) = fail "encode: Only boolean"
encode _ (Var _) = fail "encode: Only variable"
encode ct (l :== r) = uncurry (ctEqual ct) =<< (,) <$> encodeT l <*> encodeT r
encode ct c@(_ :<= _) = uncurry (ctEqual ct) =<< (,) <$> encodeT c <*> pure true

encodeT :: Exp Var t -> TcPluginM Type
encodeT (Bool False) = pure false
encodeT (Bool True) = pure true
encodeT (Var v) = pure $ var v
encodeT (Const n) = pure $ cnst n
encodeT (_ :== _) = fail "encodeT: equation"
encodeT (l :<= r) = leq <$> encodeT l <*> encodeT r
encodeT (l :+ r) = add <$> encodeT l <*> encodeT r
encodeT (l :- r) = sub <$> encodeT l <*> encodeT r
