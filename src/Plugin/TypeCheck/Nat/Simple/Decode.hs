{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.Decode (decodeAll, decode) where

import TcRnTypes
import Control.Monad
import Data.Either

import GhcPlugins (Var, promotedFalseDataCon, promotedTrueDataCon)
import TyCoRep
import Control.Monad.Trans.Except
import Data.Derivation.Expression
import TcTypeNats
import Control.Applicative ((<|>))

import Data.Except.Message
import Plugin.TypeCheck.Nat.Simple.UnNomEq

exVar :: Type -> Except Message (Exp Var a)
exVar = \case TyVarTy v -> pure $ Var v; _ -> throwE "exVar: fail"

exNum :: Type -> Except Message (Exp Var Number)
exNum (TyVarTy v) = pure $ Var v
exNum (LitTy (NumTyLit n)) = pure $ Const n
exNum (TyConApp tc [l, r])
	| tc == typeNatAddTyCon = (:+) <$> exNum l <*> exNum r
	| tc == typeNatSubTyCon = (:-) <$> exNum l <*> exNum r
exNum _ = throwE "exNum: fail"

exBool :: Type -> Except Message (Exp Var Bool)
exBool (TyVarTy v) = pure $ Var v
exBool (TyConApp tc [])
	| tc == promotedFalseDataCon = pure $ Bool False
	| tc == promotedTrueDataCon = pure $ Bool True
exBool (TyConApp tc [l, r])
	| tc == typeNatLeqTyCon = (:<=) <$> exNum l <*> exNum r
exBool _ = throwE "exBool: fail"

decodeGen :: Type -> Type -> Except Message (Exp Var Bool)
decodeGen (TyVarTy l) r = le <$> exVar r <|> le <$> exNum r <|> le <$> exBool r
	where le = (Var l :==)
decodeGen l r = (:==) <$> exNum l <*> exNum r <|> (:==) <$> exBool l <*> exBool r

decodeAll :: [Ct] -> [Exp Var Bool]
decodeAll cts = rights $ runExcept . decode <$> cts

decode :: Ct -> Except Message (Exp Var Bool)
decode = uncurry decodeGen <=< unNomEq
