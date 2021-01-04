memo
====

todo
----

* [x] repair divide-by-zero
	+ [x] make test data
	+ [x] repair bug
* [ ] refactoring

refactor
--------

* [x] check module name hierarchy
* [x] refactor haddock
	+ [x] Data.Derivation.Expression
	+ [x] Data.Derivation.Parse
	+ [x] Data.Derivation.CanDerive
	+ [x] Plugin.TypeCheck.Nat.Simple
* [x] refactor Data.Derivation.Expression.Internal
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] DATA EXP
			* [x] data Exp v t
			* [x] data Number
			* [x] instance Show (Exp v t)
			* [x] instance Outputable (Exp v t)
		- [x] CONSTRAINT
			* [x] CONSTRAINT
			* [x] PROCESS EQUATION
				+ [x] Bool _
				+ [x] Var _
				+ [x] (:<=) False
				+ [x] (:<=) True
				+ [x] (_ :== Bool _)
				+ [x] (Bool _ :== _)
				+ [x] (_ :== Var _)
				+ [x] (Var _ :== _)
				+ [x] (_ :== _) True
				+ [x] (_ :== _) False
		- [x] POLYNOMIAL
		- [x] MAP FROM VARIABLE TO BOOL
			* [x] type VarBool v
			* [x] varBool
			* [x] vbInit
			* [x] vbStep
			* [x] untilFixed
* [x] refactor Data.Derivation.Expression
* [x] refactor Data.Parse
* [ ] refactor Data.Derivation.Parse
	+ [ ] export list
	+ [ ] import list
	+ [ ] structure
	+ [ ] body
* [ ] refactor Data.Derivation.Constraint
* [ ] refactor Data.Derivation.CanDerive
* [ ] refactor Plugin.TypeCheck.Nat.Simple.Decode
* [ ] refactor Plugin.TypeCheck.Nat.Simple

module name hierarchy
---------------------

```
Data
  + Parse
  + Derivation
      + CanDerive
      + Constraint
      + Expression
      |   + itself
      |   + Internal
      + Parse
Plugin.TypeCheck.Nat.Simple
  + itself
  + Decode
```
