memo
====

todo
----

* [x] repair divide-by-zero
	+ [x] make test data
	+ [x] repair bug
* [x] make Error and Log monad
* [x] repair interface
	+ [x] make Data.Except.Message
	+ [x] others
* [x] separate StateT from Data.Parse
* [x] repair decodeAll
* [ ] repair pluginWith
* [ ] refactor haddock
* [ ] refactor

refactor
--------

* [x] check module name hierarchy
* [x] refactor haddock
	+ [x] Control.Monad.Try
		- [x] structure
		- [x] DATA TRY
		- [x] RUN TRY
		- [x] THROW AND CATCH ERROR
		- [x] WRITE AND GET LOG
		- [x] LOG STRING
	+ [x] Data.Derivation.Expression
	+ [x] Data.Derivation.Parse
	+ [x] Data.Derivation.CanDerive
	+ [x] Plugin.TypeCheck.Nat.Simple
	+ [x] Plugin.TypeCheck.Nat.Simple.PluginWith
	+ [x] Plugin.TypeCheck.Nat.Simple.Decode
* [x] refactor with hlint
* [x] refactor Control.Monad.StateT
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] NEWTYPE STATE T
		- [x] INSTANCE
			* [x] FUNCTOR
			* [x] APPLICATIVE AND ALTERNATIVE
			* [x] MONAD AND MONAD PLUS
* [ ] refactor Control.Monad.Try
	+ [x] export list
	+ [x] import list
	+ [ ] structure
	+ [ ] body
* [ ] refactor Data.Derivation.Expression.Internal
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
* [ ] refactor Data.Derivation.Expression
* [ ] refactor Data.Parse
* [ ] refactor Data.Derivation.Parse
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] PARSE CONSTRAINT
			* [x] remove function tokens
		- [x] MEMO
			* [x] data Memo
			* [x] type Var
			* [x] function memo
		- [x] GRAMMAR
			* [x] PEG
			* [x] pConstraint
			* [x] pEqual
			* [x] pBool
			* [x] pLessEqual
			* [x] pPolynomial
			* [x] pNumber
			* [x] var
		- [x] PICK AND CHECK
			* [x] pick
			* [x] check
* [ ] refactor Data.Derivation.Constraint
	* [x] export list
	* [x] import list
	* [ ] structure
	* [ ] body
* [ ] refactor Data.Derivation.CanDerive
* [ ] refactor Plugin.TypeCheck.Nat.Simple.Decode
* [ ] refactor Plugin.TypeCheck.Nat.Simple.UnNomEq
* [ ] refactor Plugin.TypeCheck.Nat.Simple.PluginWith
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
