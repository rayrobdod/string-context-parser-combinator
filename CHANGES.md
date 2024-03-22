# Changelog

## [Unreleased]
* Add symbolic operators to Parser, Extractor and Interpolator
  * `<~>` and `<|>` as aliases for `andThen` and `orElse`, respectively
  * `~>` and `<~` as specializations of `andThen` when one side or the other is Unit
  * `<+>` on Interpolator only as a specialization of `orElse` when the desired result is a discriminated union
  * `</>` on Interpolator only as a specialization of `orElse` when the second argument is a pure parser
  * `<::>` on Interpolator only as a specialization of `andThen` which prepends the left side to a list on the right side
* Add built-in but explicit Repeated instances for concatenating strings
  * `idConcatenateString` for id context
  * `forContextConcatenateString` for scala-2 macro context
  * `quotedConcatenateString` for scala-3 quoted context
* Fix scalajs sourcemaps to point at github-hosted files instead of a local file that almost certainly does not exist

## [0.1.0] 2024-02-01
Initial tagged version
