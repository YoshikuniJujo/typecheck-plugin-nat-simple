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
	ct = pConstraint `unparse` m
	eq = pEqual `unparse` m
	bl = pBool `unparse` m
	le = pLessEqual `unparse` m
	pl = pPolynomial `unparse` m
	nm = pNumber `unparse` m
	tk = (memo `second`) <$> uncons ts

---------------------------------------------------------------------------
-- GRAMMAR
---------------------------------------------------------------------------

-- Constraint <- Equal / LessEqual
-- Equal <-
--	var "==" var !("+" / "-" / "<=") /
--	var "==" Polynomial !"<=" /
--	var "==" Bool /
--	Polynomial "==" Polynomial /
--	Bool "==" Bool
-- Bool <- LessEqual / "F" / "T" / var
-- LessEqual <- Polynomial "<=" Polynomial
-- Polynomial <- Number ("+" Number / "-" Number)*
-- Number <- <digit string> / var / "(" Polynomial ")"

pConstraint :: Parse Memo (Exp Var Bool)
pConstraint = parse equal <|> parse lessEqual

pEqual :: Parse Memo (Exp Var Bool)
pEqual = (:==) <$> var <* pick "==" <*> var
		>>! (pick "+" <|> pick "-" <|> pick "<=") <|>
	(:==) <$> var <* pick "==" <*> parse polynomial >>! pick "<=" <|>
	(:==) <$> var <* pick "==" <*> parse bool <|>
	(:==) <$> parse polynomial <* pick "==" <*> parse polynomial <|>
	(:==) <$> parse bool <* pick "==" <*> parse bool

pBool :: Parse Memo (Exp Var Bool)
pBool =	parse lessEqual <|>
	Bool False <$ pick "F" <|> Bool True <$ pick "T" <|> var

pLessEqual :: Parse Memo (Exp Var Bool)
pLessEqual = (:<=) <$> parse polynomial <* pick "<=" <*> parse polynomial

pPolynomial :: Parse Memo (Exp Var Number)
pPolynomial = foldl (&) <$> parse number <*> many (
	flip (:+) <$> (pick "+" *> parse number) <|>
	flip (:-) <$> (pick "-" *> parse number) )

pNumber :: Parse Memo (Exp Var Number)
pNumber = Const . read <$> check (all isDigit) <|> var <|>
	pick "(" *> parse polynomial <* pick ")"

var :: Parse Memo (Exp Var t)
var = Var <$> check (all isLower)

---------------------------------------------------------------------------
-- PICK AND CHECK
---------------------------------------------------------------------------

pick :: String -> Parse Memo String
pick = check . (==)

check :: (String -> Bool) -> Parse Memo String
check p = parse token >>= B.bool empty <$> pure <*> p
