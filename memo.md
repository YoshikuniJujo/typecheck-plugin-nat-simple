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
* [x] refactor Data.Derivation.Expression.Internal
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] DATA EXP
			* [x] DATA
				+ [x] data Exp v t
				+ [x] data ExpType
			* [x] INSTANCE
				+ [x] instance Show (Exp v t)
				+ [x] instance Outputable (Exp v t)
				+ [x] pprOp
				+ [x] instance Loggable s v (Exp v t)
				+ [x] logOp
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
				* [x] data Constraint v
				* [x] constraint
				* [x] instance Loggable s v (Constraint v)
			- [x] CONSTRUCT
			- [x] READ
				* [x] vars
				* [x] has
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
				* [x] type Poly
				* [x] polyToLog
				* [x] polyToLog1
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
			- [x] allM
			- [x] canDerive1
		+ [x] GIVENS
			- [x] NEWTYPE GIVENS AND CONSTRUCTOR
				* [x] newtype Givens v
				* [x] instance Loggable s v (Givens v)
				* [x] givens
			- [x] GIVENS VARIABLES
			- [x] REMOVE VARIABLE
				* [x] rmVar
				* [x] rmStep
				* [x] rmVar1
				* [x] unfoldUntil
		+ [x] WANTED
			- [x] newtype Wanted v
			- [x] type Wanted1 v
			- [x] instance Loggable s v (Wanted v)
			- [x] wanted
* [x] refactor Data.Log
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] LOG
			* [x] NEWTYPE LOG
			* [x] INSTANCE
				+ [x] structure
				+ [x] Semigroup
				+ [x] Monoid
				+ [x] Show
				+ [x] Outputable
				+ [x] pprLog1
				+ [x] Message
				+ [x] messageLog1
				+ [x] IsString
				+ [x] IsSDoc
			* [x] FUNCTION
				+ [x] logVar
				+ [x] (.+.)
				+ [x] intersperse
				+ [x] unwords
					- [x] consider to use intersperse
			* [x] CLASS
				+ [x] Loggable
				+ [x] Message
		- [x] SDOC STRING
			* [x] structure
			* [x] class IsSDoc s
			* [x] data SDocStr
			* [x] instance Semigroup SDocStr
			* [x] instance Monoid SDocStr
			* [x] instance Outputable SDocStr
			* [x] instance IsString SDocStr
			* [x] instance IsSDoc SDocStr
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
* [ ] refactor Plugin.TypeCheck.Nat.Simple.TypeCheckWith
	+ [ ] export list
	+ [ ] import list
	+ [ ] body
		- [ ] typeCheckWith
		- [ ] solve
		- [ ] result
* [ ] refactor Plugin.TypeCheck.Nat.Simple
	+ [ ] export list
	+ [ ] import list
	+ [ ] function plugin

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
