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
* [x] repair pluginWith
* [x] refactor haddock
* [x] use ExpType instead of Star
* [x] try to use in renaged-list
* [x] try to use in ranged-list
* [x] try to use in ranged-list
* [ ] show v in log
	+ [x] make `Data.Log`
	+ [x] define `Log s v`
		- [x] `newtype Log s v = Log [[Either s v]]`
		- [x] (.+.) :: Log s v -> Log s v -> Log s v
	+ [x] define `instance Monoid (Log  s v)`
	+ [x] define `instance IsString s => IsString (Log s v)`
	+ [x] define `instance (Show s, Show v) => Show (Log s v)`
	+ [x] define `class Message m`
	+ [x] define `instance (Message s, Show v) => Message (Log s v)`
	+ [x] define `instance (Outputable s, Outputable v) => Outputable (Log s v)`
	+ [x] use `Log v` in error message in test
	+ [x] use `Log v` in error message
	+ [x] define `class Loggable`
	+ [x] define `instance Loggable (Exp v t)`
* [x] log value of `Exp v 'Boolean`
	+ [x] in given
	+ [x] in wanted
* [ ] refactor

refactor
--------

* [x] check module name hierarchy
* [x] consider move module Data.Log
* [x] refactor haddock
	+ [x] Control.Monad.Try
		- [x] structure
		- [x] DATA TRY
		- [x] RUN TRY
		- [x] THROW AND CATCH ERROR
		- [x] WRITE AND GET LOG
		- [x] TOOL
	+ [x] Data.Log
		- [x] make export list
		- [x] structure
		- [x] LOG
			* [x] DATA LOG
				+ [x] data Log s v
				+ [x] (.+.)
				+ [x] logVar
			* [x] CLASS
				+ [x] class Loggable s v a
				+ [x] class Message
		- [x] SDOC
			* [x] IsSDoc
			* [x] SDocStr
	+ [x] Data.Derivation.Expression
	+ [x] Data.Derivation.Parse
	+ [x] Data.Derivation.CanDerive
	+ [x] Plugin.TypeCheck.Nat.Simple
	+ [x] Plugin.TypeCheck.Nat.Simple.TypeCheckWith
	+ [x] Plugin.TypeCheck.Nat.Simple.Decode
* [ ] unify `Try s s` and `Try w w` and so on
	+ [x] in haddock
		- [x] Control.Monad.Try
		- [x] Data.Log
		- [x] Data.Derivation.Expression
		- [x] Data.Derivation.Parse
		- [x] Data.Derivation.CanDerive
		- [x] Plugin.TypeCheck.Nat.Simple
		- [x] Plugin.TypeCheck.Nat.Simple.TypeCheckWith
		- [x] Plugin.TypeCheck.Nat.Simple.Decode
	+ [ ] others
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
* [x] refactor Control.Monad.Try
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] DATA TRY
			* [x] DATA
			* [x] INSTANCE
				+ [x] Functor
				+ [x] Applicative
				+ [x] Alternative
				+ [x] Monad
				+ [x] MonadPlus
		- [x] RUN TRY
		- [x] THROW AND CATCH ERROR
			* [x] throw
			* [x] catch
			* [x] rights
		- [x] WRITE AND GET LOG
		- [x] TOOL
* [x] refactor Data.Derivation.Expression.Internal
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] DATA EXP
			* [x] data Exp v t
			* [x] data ExpType
			* [x] instance Show (Exp v t)
			* [x] instance Outputable (Exp v t)
			* [x] pprOp
			* [x] instance Loggable s v (Exp v t)
			* [x] logOp
		- [x] CONSTRAINT
			* [x] CONSTRAINT
			* [x] PROCESS EQUATION
				+ [x] type declaration
				+ [x] Bool _
				+ [x] Var _
				+ [x] (_ :<= _) False
				+ [x] (_ :<= _) True
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
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [ ] body
		- [x] PARSE CONSTRAINT
		- [x] MEMO
			* [x] data Memo
			* [x] type M
			* [x] type Var
			* [x] function memo
		- [ ] GRAMMAR
			* [ ] PEG
			* [ ] pConstraint
			* [ ] pEqual
			* [ ] pBool
			* [ ] pLessEqual
			* [ ] pPolynomial
			* [ ] pNumber
			* [ ] var
		- [ ] PICK AND CHECK
			* [ ] pick
			* [ ] check
* [ ] refactor Data.Derivation.Constraint
	* [ ] export list
	* [ ] import list
	* [ ] structure
	* [ ] body
		+ [ ] CONSTRAINT
			- [ ] DATA CONSTRAINT
				* [ ] data Constraint v
			- [ ] CONSTRUCT
				* [ ] constructor equal
				* [ ] constructor greatEqualThan
				* [ ] constructor greatThan
			- [ ] READ
				* [ ] vars
				* [ ] hasVar
				* [ ] selfContained
				* [ ] isDerivFrom
			- [ ] CONVERT
				* [ ] positives
				* [ ] eliminate
				* [ ] type Aligned
				* [ ] alignEE
				* [ ] alignEG
				* [ ] alignGG
		+ [ ] POLYNOMIAL
			- [ ] TYPE POLY
			- [ ] CONSTRUCT
			- [ ] READ
				* [ ] isGeqThan
			- [ ] CONVERT
				* [ ] posit
				* [ ] reduce
				* [ ] mul
				* [ ] divide
* [ ] refactor Data.Derivation.CanDerive
	* [ ] export list
	* [ ] import list
	* [ ] structure
	* [ ] body
		+ [ ] CAN DERIVE
			- [ ] canDerive
			- [ ] canDerive1
		+ [ ] GIVEN
			- [ ] NEWTYPE GIVEN AND CONSTRUCTOR
				* [ ] newtype Given v
				* [ ] given
			- [ ] GIVEN VARIABLES
			- [ ] REMOVE VARIABLE
				* [ ] rmVar
				* [ ] rmStep
				* [ ] rmVar1
				* [ ] unfoldUntil
		+ [ ] WANTED
			- [ ] newtype Wanted v
			- [ ] type Wanted1 v
			- [ ] wanted
* [ ] refactor Plugin.TypeCheck.Nat.Simple.Decode
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] DECODE
			* [x] decodeAll
			* [x] decode
			* [x] decodeTs
		- [x] BOOL, NUMBER AND VARIABLE
			* [x] exBool
			* [x] exNum
* [ ] refactor Plugin.TypeCheck.Nat.Simple.UnNomEq
	+ [x] export list
	+ [x] import list
	+ [x] rename PluginWith to TypeCheckWith
	+ [x] rename pluginWith to typeCheckWith
	+ [x] structure
	+ [x] body
		- [x] typeCheckWith
		- [x] solve
		- [x] result
* [ ] refactor Plugin.TypeCheck.Nat.Simple
	+ [x] export list
	+ [x] import list
	+ [x] function plugin

module name hierarchy
---------------------

```
Control.Monad
  + StateT
  + Try
Data
  + Parse
  + Log
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
  + PluginWith
  + UnNOmEq
```
