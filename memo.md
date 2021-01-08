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
		- [x] LOG STRING
			* [x] MESSAGE
			* [x] SDOC STRING
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
			- [x] CONSTRUCT
				* [x] constructor equal
				* [x] constructor greatEqualThan
				* [x] constructor greatThan
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
				* [x] isGeqThan
			- [x] CONVERT
				* [x] posit
				* [x] reduce
				* [x] mul
				* [x] divide
* [ ] refactor Data.Derivation.CanDerive
	* [x] export list
	* [x] import list
	* [x] structure
	* [ ] body
		+ [x] CAN DERIVE
			- [x] canDerive
			- [x] canDerive1
		+ [ ] GIVEN
			- [ ] NEWTYPE GIVEN AND CONSTRUCTOR
			- [ ] GIVEN VARIABLES
			- [ ] REMOVE VARIABLE
		+ [ ] WANTED
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
