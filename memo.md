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
* [x] instance Loggable Wanted
+ [x] log wanted creation
* [x] instance Loggable Given
+ [x] log givens creation
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

* [x] refactor haddock
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
	+ [x] Data.Derivation.CanDerive
		- [x] CAN DERIVE
		- [x] GIVENS
		- [x] WANTED
	+ [x] Plugin.TypeCheck.Nat.Simple
	+ [x] Plugin.TypeCheck.Nat.Simple.TypeCheckWith
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
			* [x] runTry
			* [x] gatherSuccess
		- [x] THROW AND CATCH ERROR
			* [x] throw
			* [x] catch
			* [x] rights
		- [x] WRITE AND GET LOG
			* [x] class Set x xs
			* [x] instance Set ...
			* [x] tell
			* [x] partial
		- [x] TOOL
* [ ] refactor Data.Derivation.Expression.Internal
	+ [ ] export list
	+ [ ] import list
	+ [ ] structure
	+ [ ] body
		- [ ] DATA EXP
			* [ ] data Exp v t
			* [ ] data ExpType
			* [ ] instance Show (Exp v t)
			* [ ] instance Outputable (Exp v t)
			* [ ] pprOp
			* [ ] instance Loggable s v (Exp v t)
			* [ ] logOp
		- [ ] CONSTRAINT
			* [ ] CONSTRAINT
			* [ ] PROCESS EQUATION
				+ [ ] type declaration
				+ [ ] Bool _
				+ [ ] Var _
				+ [ ] (_ :<= _) False
				+ [ ] (_ :<= _) True
				+ [ ] (_ :== Bool _)
				+ [ ] (Bool _ :== _)
				+ [ ] (_ :== Var _)
				+ [ ] (Var _ :== _)
				+ [ ] (_ :== _) True
				+ [ ] (_ :== _) False
		- [ ] POLYNOMIAL
		- [ ] MAP FROM VARIABLE TO BOOL
			* [ ] type VarBool v
			* [ ] varBool
			* [ ] vbInit
			* [ ] vbStep
			* [ ] untilFixed
* [ ] refactor Data.Derivation.Expression
* [ ] refactor Data.Parse
* [ ] refactor Data.Derivation.Parse
	+ [ ] export list
	+ [ ] import list
	+ [ ] structure
	+ [ ] body
		- [ ] PARSE CONSTRAINT
		- [ ] MEMO
			* [ ] data Memo
			* [ ] type M
			* [ ] type Var
			* [ ] function memo
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
			- [ ] CONSTRUCT
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
