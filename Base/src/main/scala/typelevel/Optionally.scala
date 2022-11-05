package com.rayrobdod.stringContextParserCombinator
package typelevel

/**
 * Describes how to represent an optional value
 *
 * @tparam A the optional input elements
 * @tparam Z the result container
 */
trait Optionally[-A, +Z] {
	/** Returns a `Z` value representing a missing `A` */
	def none:Z
	/** Returns a `Z` value representing the given `A` */
	def some(elem:A):Z
}

/** Predefined implicit implementations of Optionally */
object Optionally extends LowPrioOptionally {
	def apply[A, Z](noneFn:Z, someFn:A => Z):Optionally[A, Z] = {
		final class Apply extends Optionally[A, Z] {
			def none:Z = noneFn
			def some(elem:A):Z = someFn(elem)
		}
		new Apply
	}

	/**
	 * An Optionally in which a provided value is used as-is, and `default` is used if the value is missing
	 */
	def whereDefault[A](default:A):Optionally[A, A] = this.apply[A, A](default, Predef.identity _)

	implicit def optionallyUnit:Optionally[Unit, Unit] = this.whereDefault(())
}

private[typelevel] trait LowPrioOptionally {
	implicit def optionallyGeneric[A]:Optionally[A, Option[A]] = Optionally(None, Some.apply _)
}
