# Changelog

## [Unreleased](https://github.com/rayrobdod/string-context-parser-combinator/compare/0.1.1...HEAD)
* Improve performance of `codePointWhere` and `charWhere` if default error message is not needed
* Fix `CodePoint::isIdentifierIgnorable` to forward to the correct `j.l.Character` method
* Flatten typeclass's companion's inheritance hierarchy
  * Include pages for the companion's scala-2 `Context` inner classes in scala-2 docs
    (was previously not included since, despite being visible via inheritance, class was still an inner class of a package-private trait)
  * Use `ifdef` annotation instead of inheritance for scala-version-specific methods
* Flatten Interpolator, Extractor, Parser and companions' inheritance hierarchy
  * Include pages for `Interpolator.LiftedInterpolator` inner classes in docs
  * Use `ifdef` annotation instead of inheritance for scala-version-specific methods

## [0.1.1](https://github.com/rayrobdod/string-context-parser-combinator/compare/0.1.0...0.1.1) – 2025-02-04
* Add symbolic operators to Parser, Extractor and Interpolator
  * `<~>` and `<|>` as aliases for `andThen` and `orElse`, respectively
  * `~>` and `<~` as specializations of `andThen` when one side or the other is Unit
  * `<+>` on Interpolator only as a specialization of `orElse` when the desired result is a discriminated union
  * `</>` on Interpolator only as a specialization of `orElse` when the second argument is a pure parser
  * `<::>` on Interpolator only as a specialization of `andThen` which prepends the left side to a list on the right side
* Add built-in but explicit Repeated instances for concatenating strings
  * `idConcatenateString` for id context
  * `forContext(c).concatenateString` for scala-2 macro context
  * `quotedConcatenateString` for scala-3 quoted context
* Add `Repeat.SplicePiece`; representing either zero, one, or many items; and typeclass instances using `SplicePiece`
  * for scala-2 macro context:
    * `Either.forContext(c).splicePiece` to combine a `Interpolator[c.Expr[A]]` and a `Interpolator[c.Expr[List[A]]]` into a `Interpolator[SplicePiece[c.Expr, A]]`
    * `Repeated.forContext(c).fromSplicesToExprList` to splice the SplicePieces together into a `c.Expr[List[A]]`
    * `Repeated.forContext(c).fromSplicesUsingBuilder` to splice the SplicePieces together using a arbitrary Builder
  * for scala-3 quoted context:
    * `Either.quotedSplicePiece` to combine a `Interpolator[Expr[A]]` and a `Interpolator[Expr[List[A]]]` into a `Interpolator[SplicePiece[Expr, A]]`
    * `Repeated.quotedFromSplicesToExprList` to splice the SplicePieces together into a `Expr[List[A]]`
    * `Repeated.quotedFromSplicesUsingBuilder` to splice the SplicePieces together using a arbitrary Builder
* Fix scalajs sourcemaps to point at github-hosted files instead of a local file that almost certainly does not exist

## [0.1.0](https://github.com/rayrobdod/string-context-parser-combinator/releases/tag/0.1.0) – 2024-02-01
Initial tagged version
