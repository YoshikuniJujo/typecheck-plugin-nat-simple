{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Derivation.CanDerive
import Data.Derivation.Parse

main :: IO ()
main = do
	print "foo" -- $ wanted =<< parseConstraint "0 == n - n"
