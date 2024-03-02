# Changelog

## [Unreleased]
* Add symbolic operators to Parser, Extractor and Interpolator
  * `<~>` and `<|>` as aliases for `andThen` and `orElse`, respectively
  * `~>` and `<~` as specializations of `andThen` when one side or the other is Unit
  * `<+>` on Interpolator only as a specialization of `orElse` when the desired result is a discriminated union
  * `</>` on Interpolator only as a specialization of `orElse` when the second argument is a pure parser
  * `<::>` on Interpolator only as a specialization of `andThen` which prepends the left side to a list on the right side

## [0.1.0] 2024-02-01
Initial tagged version
