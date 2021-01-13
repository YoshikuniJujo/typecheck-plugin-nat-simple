{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.Decode (
	-- * DECODE CT
	decodeAll, decode ) where

import GhcPlugins (Var, promotedFalseDataCon, promotedTrueDataCon, text, (<+>), ppr)
import TcRnTypes (Ct)
import TcTypeNats (typeNatAddTyCon, typeNatSubTyCon, typeNatLeqTyCon)
import TyCoRep (Type(..), TyLit(..))
import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Control.Monad.Try (Try, throw, rights, Set)
import Data.Derivation.Expression (Exp(..), ExpType(..))

import Plugin.TypeCheck.Nat.Simple.UnNomEq (unNomEq)

import Data.Log

---------------------------------------------------------------------------

-- * DECODE
-- * BOOL, NUMBER AND VARIABLE

---------------------------------------------------------------------------
-- DECODE
---------------------------------------------------------------------------

decodeAll ::
	(Monoid w, IsSDoc w, Set w w) => [Ct] -> Try w w [Exp Var 'Boolean]
decodeAll = rights . (decode <$>)

decode :: (Monoid w, IsSDoc w) => Ct -> Try w w (Exp Var 'Boolean)
decode = uncurry decodeTs <=< unNomEq

decodeTs :: (Monoid s, IsSDoc s) => Type -> Type -> Try s s (Exp Var 'Boolean)
decodeTs (TyVarTy l) (TyVarTy r) = pure $ Var l :== Var r
decodeTs l r = (:==) <$> exNum l <*> exNum r <|> (:==) <$> exBool l <*> exBool r

---------------------------------------------------------------------------
-- BOOL, NUMBER AND VARIABLE
---------------------------------------------------------------------------

exBool :: (Monoid s, IsSDoc e) => Type -> Try e s (Exp Var 'Boolean)
exBool (TyVarTy v) = pure $ Var v
exBool (TyConApp tc [])
	| tc == promotedFalseDataCon = pure $ Bool False
	| tc == promotedTrueDataCon = pure $ Bool True
exBool (TyConApp tc [l, r])
	| tc == typeNatLeqTyCon = (:<=) <$> exNum l <*> exNum r
exBool t = throw . fromSDoc $ text "exBool: not boolean" <+> ppr t

exNum :: (Monoid s, IsSDoc e) => Type -> Try e s (Exp Var 'Number)
exNum (TyVarTy v) = pure $ Var v
exNum (LitTy (NumTyLit n)) = pure $ Const n
exNum (TyConApp tc [l, r])
	| tc == typeNatAddTyCon = (:+) <$> exNum l <*> exNum r
	| tc == typeNatSubTyCon = (:-) <$> exNum l <*> exNum r
exNum t = throw . fromSDoc $ text "exNum: not number" <+> ppr t
