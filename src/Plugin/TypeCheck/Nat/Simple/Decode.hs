{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.Decode (decodeAll, decode) where

import TcRnTypes
import Control.Monad

import GhcPlugins (Var, promotedFalseDataCon, promotedTrueDataCon)
import TyCoRep
import Data.Derivation.Expression
import TcTypeNats
import Control.Applicative ((<|>))

import Plugin.TypeCheck.Nat.Simple.UnNomEq

import Control.Monad.Try
import Data.String

exVar :: (Monoid s, IsString e) => Type -> Try e s (Exp Var a)
exVar = \case TyVarTy v -> pure $ Var v; _ -> throw "exVar: fail"

exNum :: (Monoid s, IsString e) => Type -> Try e s (Exp Var Number)
exNum (TyVarTy v) = pure $ Var v
exNum (LitTy (NumTyLit n)) = pure $ Const n
exNum (TyConApp tc [l, r])
	| tc == typeNatAddTyCon = (:+) <$> exNum l <*> exNum r
	| tc == typeNatSubTyCon = (:-) <$> exNum l <*> exNum r
exNum _ = throw "exNum: fail"

exBool :: (Monoid s, IsString s, IsString e) => Type -> Try e s (Exp Var Bool)
exBool (TyVarTy v) = pure $ Var v
exBool (TyConApp tc [])
	| tc == promotedFalseDataCon = pure $ Bool False
	| tc == promotedTrueDataCon = pure $ Bool True
exBool (TyConApp tc [l, r])
	| tc == typeNatLeqTyCon = (:<=) <$> exNum l <*> exNum r
exBool _ = throw "exBool: fail"

decodeGen :: (Monoid s, IsString s, Monoid e, IsString e) => Type -> Type -> Try e s (Exp Var Bool)
decodeGen (TyVarTy l) r = le <$> exVar r <|> le <$> exNum r <|> le <$> exBool r
	where le = (Var l :==)
decodeGen l r = (:==) <$> exNum l <*> exNum r <|> (:==) <$> exBool l <*> exBool r

decodeAll :: (Monoid s, IsString s, Set s s) => [Ct] -> Try s s [Exp Var Bool]
decodeAll cts = rights $ decode <$> cts

decode :: (Monoid s, IsString s, Set s s, Monoid e, IsString e) => Ct -> Try e s (Exp Var Bool)
decode = uncurry decodeGen <=< unNomEq
