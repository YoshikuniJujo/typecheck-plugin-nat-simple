{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Result where

resultAnd :: [Result a] -> Result [a]
resultAnd [] = Ok
resultAnd (Ok : rs) = resultAnd rs
resultAnd (Err x : rs) = case resultAnd rs of
	Ok -> Err [x]
	Err xs -> Err $ x : xs

data Result a = Ok | Err a deriving Show

isOk :: Result a -> Bool
isOk Ok = True
isOk (Err _) = False

result :: b -> (a -> b) -> Result a -> b
result o _ Ok = o
result _ f (Err x) = f x

instance Functor Result where
	_ `fmap` Ok = Ok
	f `fmap` Err x = Err $ f x
