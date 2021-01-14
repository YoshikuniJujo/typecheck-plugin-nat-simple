{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}
-- {-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.TypeNats
import Data.Proxy

main :: IO ()
main = print $ foo Proxy

foo :: (n + 1) ~ (m + 1) => Proxy n -> Proxy m
foo = id
