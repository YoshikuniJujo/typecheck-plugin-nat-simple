{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Parse (
	-- * PARSE CONSTRAINT
	parseConstraint, Var ) where

import Control.Applicative (empty, many, (<|>))
import Control.Arrow (second)
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.List (uncons, unfoldr)
import Data.Char (isDigit, isLower)
import Data.Parse (Parse, parse, unparse, (>>!))
import Data.Derivation.Expression (Exp(..), Number)

import qualified Data.Bool as B (bool)

---------------------------------------------------------------------------

-- * PARSE CONSTRAINT
-- * MEMO
-- * GRAMMAR
-- * PICK AND CHECK

---------------------------------------------------------------------------
-- PARSE CONSTRAINT
---------------------------------------------------------------------------

parseConstraint :: String -> Maybe (Exp Var Bool)
parseConstraint = (fst <$>) . constraint . memo . unfoldr (listToMaybe . lex)

---------------------------------------------------------------------------
-- MEMO
---------------------------------------------------------------------------

data Memo = Memo {
	constraint :: Maybe (Exp Var Bool, Memo),
	equal :: Maybe (Exp Var Bool, Memo),
	bool :: Maybe (Exp Var Bool, Memo),
	lessEqual :: Maybe (Exp Var Bool, Memo),
	polynomial :: Maybe (Exp Var Number, Memo),
	number :: Maybe (Exp Var Number, Memo),
	token :: Maybe (String, Memo) }

type Var = String

memo :: [String] -> Memo
memo ts = m where
	m = Memo ct eq bl le pl nm tk
	ct = unparse pConstraint m
	eq = unparse pEqual m
	bl = unparse pBool m
	le = unparse pLessEqual m
	pl = unparse pPolynomial m
	nm = unparse pNumber m
	tk = (memo `second`) <$> uncons ts

---------------------------------------------------------------------------
-- GRAMMAR
---------------------------------------------------------------------------

pConstraint :: Parse Memo (Exp Var Bool)
pConstraint = parse equal <|> parse lessEqual

pEqual :: Parse Memo (Exp Var Bool)
pEqual =
	(:==) <$> var <* pick "==" <*> var
		>>! (pick "+" <|> pick "-" <|> pick "<=") <|>
	(:==) <$> var <* pick "==" <*> parse polynomial >>! pick "<=" <|>
	(:==) <$> var <* pick "==" <*> parse bool <|>
	(:==) <$> parse polynomial <* pick "==" <*> parse polynomial <|>
	(:==) <$> parse bool <* pick "==" <*> parse bool
	where var = Var <$> check (all isLower)

pBool :: Parse Memo (Exp Var Bool)
pBool =	parse lessEqual <|>
	Bool False <$ pick "F" <|> Bool True <$ pick "T" <|>
	Var <$> check (all isLower)

pLessEqual :: Parse Memo (Exp Var Bool)
pLessEqual = (:<=) <$> parse polynomial <* pick "<=" <*> parse polynomial

pPolynomial :: Parse Memo (Exp Var Number)
pPolynomial = foldl (&) <$> parse number <*> many (
	flip (:+) <$> (pick "+" *> parse number) <|>
	flip (:-) <$> (pick "-" *> parse number) )

pNumber :: Parse Memo (Exp Var Number)
pNumber =
	Const . read <$> check (all isDigit) <|> Var <$> check (all isLower) <|>
	pick "(" *> parse polynomial <* pick ")"

---------------------------------------------------------------------------
-- PICK AND CHECK
---------------------------------------------------------------------------

pick :: String -> Parse Memo String
pick = check . (==)

check :: (String -> Bool) -> Parse Memo String
check p = parse token >>= \t -> B.bool empty (pure t) (p t)
