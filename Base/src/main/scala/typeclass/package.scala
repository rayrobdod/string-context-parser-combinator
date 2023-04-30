package com.rayrobdod.stringContextParserCombinator

/**
 * Implicit values used by branch combinators
 * that allow combinations of input types to have more ergonomic return types.
 *
 * Each of the typeclasses defined in this package fit into a matrix,
 * where one dimension is which type of parser the typeclass is used with
 * and the other dimension is the method that uses an instance of the type
 *
 * | method | Covariant ([[Interpolator]]) | Contravariant ([[Extractor]]) | Invariant ([[Parser]]) |
 * | - | --- | --- | --- |
 * | `andThen` | [[Sequenced]] | [[ContraSequenced]] | [[BiSequenced]] |
 * | `orElse` | [[Eithered]] | [[ContraEithered]] | [[BiEithered]] |
 * | `repeat` | [[Repeated]] | [[ContraRepeated]] | [[BiRepeated]] |
 * | `optionally` | [[Optionally]] | [[ContraOptionally]] | [[BiOptionally]] |
 *
 * Thus, if you are only working with interpolators, then you'll only need to work with the unprefixed typeclasses.
 *
 * Each of these traits has a companion object that defines a generic instance of the trait,
 * and a few instances for more specific types.
 * For instance, each typeclass includes a instance that will avoid wrapping `scala.Unit` values in a collection or tuple.
 *
 * Defining custom instances of these types is supported.
 * Making custom given instances can significantly reduce the number of  explicit `map` calls required when writing a parser,
 * however the usual advice with [[https://docs.scala-lang.org/scala3/reference/contextual/givens.html given instances]] applies:
 * keep types specific, or keep the scope of a given instance to the minimum viable to prevent given instances from becoming confusing.
 */
package object typeclass {
}
