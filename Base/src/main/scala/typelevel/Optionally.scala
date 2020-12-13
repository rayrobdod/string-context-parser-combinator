package com.rayrobdod.stringContextParserCombinator
package typelevel

/**
 * Describes how to represent an optional value
 */
trait Optionally[-A, +Z] {
	/** Returns a `Z` value representing a missing `A` */
	def none:Z
	/** Returns a `Z` value representing the given `A` */
	def some(elem:A):Z
}

/** Predefined implicit implementations of Optionally */
object Optionally extends LowPrioOptionally {
	implicit def optionallyUnit:Optionally[Unit, Unit] = new OptionallyUnit
	private[this] final class OptionallyUnit extends Optionally[Unit, Unit] {
		def none:Unit = ()
		def some(elem:Unit):Unit = elem
	}
}

private[typelevel] trait LowPrioOptionally {
	implicit def optinallyGeneric[A]:Optionally[A, Option[A]] = new OptinallyGeneric[A]
	private[this] final class OptinallyGeneric[A] extends Optionally[A, Option[A]] {
		def none:Option[A] = None
		def some(elem:A):Option[A] = Some(elem)
	}
}
