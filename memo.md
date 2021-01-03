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
* [ ] refactor haddock
	+ [x] Data.Derivation.Expression
	+ [ ] Data.Derivation.Parse
	+ [ ] Data.Derivation.CanDerive
	+ [ ] Plugin.TypeCheck.Nat.Simple
* [ ] refactor Data.Derivation.Expression.Internal
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
