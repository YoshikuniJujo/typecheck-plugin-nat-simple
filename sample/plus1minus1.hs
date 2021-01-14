{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

import GHC.TypeNats
import Data.Proxy

main :: IO ()
main = print $ foo @123 Proxy

foo :: 1 <= n => Proxy (n + 1 - 1) -> Proxy (n - 1 + 1)
foo = id
