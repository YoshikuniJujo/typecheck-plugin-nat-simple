{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

import GHC.TypeNats
import Data.Proxy

main :: IO ()
main = putStrLn "Slozsoft"

foo :: Proxy (n - 1 + 1) -> Proxy (n + 1)
foo = id
