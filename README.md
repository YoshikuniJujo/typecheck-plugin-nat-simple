# typecheck-plugin-nat-simple

## what's this

This package provide plugin which extend type checking of Nat.
The type checker can calculate only addition and subtraction of constants and variables.

(View sample code on directory sample/.)

## motivation

Suppose you need lengthed list. You define like following.
(View `sample/lengthed_tail.hs`)

```haskell
import GHC.TypeNats

infixr 6 :.

data List :: Nat -> * -> * where
        Nil :: List 0 a
	(:.) :: a -> List ln a -> List (ln + 1) a
```

And you want to define function `tail`.

```haskell
tail_ :: List (n + 1) a -> List n a
tail_ Nil = error "tail_: Nil"
tail_ (_ :. xs) = xs
```

But it cause type check error.

```
error:
  ・Could not deduce: ln ~ n
    from the context: (n + 1) ~ (ln + 1)
    ...
```

Type checker say "I cannot derive (ln == n) from (n + 1 == ln + 1)".
But it seems to be obvious.
You can use this plugin like following.

```haskell
{-# OPTIONS_GHC -fplugin=Plugin.TypeCheck.Nat.Simple #-}
```

Add it to top of the code, then type check success.

## more example

To show more example, I will use Data.Proxy.Proxy.
First examle is following (View `sample/mplus1_nplus1`).

```haskell
foo :: (m + 1) ~ (n + 1) => Proxy m -> Proxy n
foo = id
```

If you don't use this plugin, then following error occur.

```
  ・Could not deduce: m ~ n
    from the context: (m + 1) ~ (n + 1)
    ...
```

Use this plugin, you can compile it.

## error and recovery

## more complex example
