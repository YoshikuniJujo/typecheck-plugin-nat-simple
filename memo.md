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
* [x] show v in log
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
* [x] try to use in ranged-list
* [x] refactor
* [x] add log to canDerive
* [ ] instance Loggable Wanted
	+ [ ] log wanted creation
* [ ] instance Loggable Given
	+ [ ] log givens creation
* [ ] refactor
* [ ] make samples
	+ [x] try to add sample/foo.txt
	+ [ ] samples using Proxy
		- [x] simple `n + 1 - 1 == n - 1 + 1`
		- [x] `m + 1 == n + 1 => m == n`
		- [ ] others
	+ [x] remove sample/foo.txt
	+ [ ] simplified Range samples
	+ [ ] samples which is part of fingertree
	+ [ ] others
* [ ] make GitHub README.md
* [ ] upload to Hackage
* [ ] register with Stackage
* [ ] refactor

refactor
--------

* [ ] refactor haddock
	+ [x] Control.Monad.Try
		- [x] structure
		- [x] DATA TRY
		- [x] RUN TRY
		- [x] THROW AND CATCH ERROR
		- [x] WRITE AND GET LOG
		- [x] TOOL
	+ [x] Data.Log
		- [x] structure
		- [x] LOG
			* [x] DATA LOG
				+ [x] data Log s v
				+ [x] logVar
				+ [x] (.+.)
				+ [x] intersperse
				+ [x] unwords
			* [x] CLASS
				+ [x] class Loggable s v a
				+ [x] class Message
		- [x] SDOC
			* [x] IsSDoc
			* [x] SDocStr
	+ [x] Data.Derivation.Expression
		- [x] data Exp
		- [x] data ExpType
	+ [x] Data.Derivation.Parse
	+ [ ] Data.Derivation.CanDerive
		- [x] CAN DERIVE
		- [x] GIVENS
		- [ ] WANTED
	+ [ ] Plugin.TypeCheck.Nat.Simple
	+ [ ] Plugin.TypeCheck.Nat.Simple.TypeCheckWith
	+ [ ] Plugin.TypeCheck.Nat.Simple.Decode
* [ ] refactor with hlint
* [ ] refactor Control.Monad.StateT
	+ [ ] export list
	+ [ ] import list
	+ [ ] structure
	+ [ ] body
		- [ ] NEWTYPE STATE T
		- [ ] INSTANCE
			* [ ] FUNCTOR
			* [ ] APPLICATIVE AND ALTERNATIVE
			* [ ] MONAD AND MONAD PLUS
* [ ] refactor Control.Monad.Try
	+ [ ] export list
	+ [ ] import list
	+ [ ] structure
	+ [ ] body
		- [ ] DATA TRY
			* [ ] DATA
			* [ ] INSTANCE
				+ [ ] Functor
				+ [ ] Applicative
				+ [ ] Alternative
				+ [ ] Monad
				+ [ ] MonadPlus
		- [ ] RUN TRY
		- [ ] THROW AND CATCH ERROR
			* [ ] throw
			* [ ] catch
			* [ ] rights
		- [ ] WRITE AND GET LOG
		- [ ] TOOL
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
* [x] refactor Data.Derivation.Parse
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] PARSE CONSTRAINT
		- [x] MEMO
			* [x] data Memo
			* [x] type M
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
* [x] refactor Data.Derivation.Constraint
	* [x] export list
	* [x] import list
	* [x] structure
	* [x] body
		+ [x] CONSTRAINT
			- [x] DATA CONSTRAINT
			- [x] CONSTRUCT
			- [x] READ
				* [x] vars
				* [x] hasVar
				* [x] selfContained
				* [x] isDerivFrom
			- [x] CONVERT
				* [x] positives
				* [x] eliminate
				* [x] type Aligned
				* [x] alignEE
				* [x] alignEG
				* [x] alignGG
		+ [x] POLYNOMIAL
			- [x] TYPE POLY
			- [x] CONSTRUCT
			- [x] READ
			- [x] CONVERT
				* [x] posit
				* [x] reduce
				* [x] mul
				* [x] divide
* [x] refactor Data.Derivation.CanDerive
	* [x] export list
	* [x] import list
	* [x] structure
	* [x] body
		+ [x] CAN DERIVE
			- [x] canDerive
			- [x] canDerive1
		+ [x] GIVEN
			- [x] NEWTYPE GIVEN AND CONSTRUCTOR
				* [x] newtype Given v
				* [x] given
			- [x] GIVEN VARIABLES
			- [x] REMOVE VARIABLE
				* [x] rmVar
				* [x] rmStep
				* [x] rmVar1
				* [x] unfoldUntil
		+ [x] WANTED
			- [x] newtype Wanted v
			- [x] type Wanted1 v
			- [x] wanted
* [ ] refactor Data.Log
	+ [ ] export list
	+ [ ] import list
	+ [ ] structure
	+ [ ] body
		- [ ] LOG
			* [ ] NEWTYPE LOG
			* [ ] INSTANCE
				+ [ ] structure
				+ [ ] Semigroup
				+ [ ] Monoid
				+ [ ] Show
				+ [ ] Outputable
				+ [ ] pprLog1
				+ [ ] Message
				+ [ ] messageLog1
				+ [ ] IsString
				+ [ ] IsSDoc
			* [ ] FUNCTION
				+ [ ] (.+.)
				+ [ ] intersperse
				+ [ ] unwords
					- [ ] consider to use intersperse
					- [ ] others
				+ [ ] logVar
			* [ ] CLASS
				+ [ ] Loggable
				+ [ ] Message
		- [ ] SDOC STRING
			* [ ] structure
			* [ ] class IsSDoc s
			* [ ] data SDocStr
			* [ ] instance Semigroup SDocStr
			* [ ] instance Monoid SDocStr
			* [ ] instance Outputable SDocStr
			* [ ] instance IsString SDocStr
			* [ ] instance IsSDoc SDocStr
* [x] refactor Plugin.TypeCheck.Nat.Simple.Decode
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] DECODE
			* [x] decodeAll
			* [x] decode
			* [x] decodeTs
		- [x] BOOLEAN AND NUMBER
			* [x] exBool
			* [x] exNum
* [x] refactor Plugin.TypeCheck.Nat.Simple.UnNomEq
	+ [x] export list
	+ [x] import list
	+ [x] body
* [x] refactor Plugin.TypeCheck.Nat.Simple.TypeCheckWith
	+ [x] export list
	+ [x] import list
	+ [x] body
		- [x] typeCheckWith
		- [x] solve
		- [x] result
* [x] refactor Plugin.TypeCheck.Nat.Simple
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
