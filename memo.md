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
* [ ] refactor Data.Derivation.Expression.Internal
	+ [x] export list
	+ [ ] import list
	+ [ ] structure
	+ [ ] body
* [ ] refactor Data.Derivation.Expression
* [ ] refactor Data.Parse
* [ ] refactor Data.Derivation.Parse
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
