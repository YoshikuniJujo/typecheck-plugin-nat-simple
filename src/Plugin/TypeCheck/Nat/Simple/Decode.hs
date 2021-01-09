{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.Decode (decodeAll, decode) where

import GhcPlugins (Var, promotedFalseDataCon, promotedTrueDataCon)
import TcRnTypes (Ct)
import TcTypeNats (typeNatAddTyCon, typeNatSubTyCon, typeNatLeqTyCon)
import TyCoRep (Type(..), TyLit(..))
import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Control.Monad.Try (Try, throw, rights, Set)
import Data.String (IsString)
import Data.Derivation.Expression (Exp(..), ExpType(..))

import Plugin.TypeCheck.Nat.Simple.UnNomEq (unNomEq)

---------------------------------------------------------------------------

-- * DECODE
-- * BOOL, NUMBER AND VARIABLE

---------------------------------------------------------------------------
-- DECODE
---------------------------------------------------------------------------

decodeAll ::
	(Monoid s, IsString s, Set s s) => [Ct] -> Try s s [Exp Var 'Boolean]
decodeAll = rights . (decode <$>)

decode :: (Monoid s, IsString s, Set s s, Monoid e, IsString e) =>
	Ct -> Try e s (Exp Var 'Boolean)
decode = uncurry decodeTs <=< unNomEq

decodeTs :: (Monoid s, IsString s, Monoid e, IsString e) =>
	Type -> Type -> Try e s (Exp Var 'Boolean)
decodeTs (TyVarTy l) (TyVarTy r) = pure $ Var l :== Var r
decodeTs l r = (:==) <$> exNum l <*> exNum r <|> (:==) <$> exBool l <*> exBool r

---------------------------------------------------------------------------
-- BOOL, NUMBER AND VARIABLE
---------------------------------------------------------------------------

exBool :: (Monoid s, IsString s, IsString e) => Type -> Try e s (Exp Var 'Boolean)
exBool (TyVarTy v) = pure $ Var v
exBool (TyConApp tc [])
	| tc == promotedFalseDataCon = pure $ Bool False
	| tc == promotedTrueDataCon = pure $ Bool True
exBool (TyConApp tc [l, r])
	| tc == typeNatLeqTyCon = (:<=) <$> exNum l <*> exNum r
exBool _ = throw "exBool: fail"

exNum :: (Monoid s, IsString e) => Type -> Try e s (Exp Var 'Number)
exNum (TyVarTy v) = pure $ Var v
exNum (LitTy (NumTyLit n)) = pure $ Const n
exNum (TyConApp tc [l, r])
	| tc == typeNatAddTyCon = (:+) <$> exNum l <*> exNum r
	| tc == typeNatSubTyCon = (:-) <$> exNum l <*> exNum r
exNum _ = throw "exNum: fail"
