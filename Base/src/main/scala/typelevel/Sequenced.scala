package com.rayrobdod.stringContextParserCombinator
package typelevel

/**
 * Describes how to combine two adjacent values into one value
 *
 * @tparam A the first input
 * @tparam B the second input
 * @tparam Z the result container
 */
@FunctionalInterface
trait Sequenced[-A, -B, +Z] {
	def aggregate(left:A, right:B):Z
}

/** Predefined implicit implementations of Sequenced */
object Sequenced extends LowPrioSequenced {
	private[typelevel] def apply[A, B, Z](fn:(A, B) => Z):Sequenced[A, B, Z] = {
		final class Apply extends Sequenced[A, B, Z] {
			def aggregate(left:A, right:B):Z = fn(left, right)
		}
		new Apply()
	}

	implicit def sequencedUnitUnit:Sequenced[Unit, Unit, Unit] = apply((_:Unit, _:Unit) => ())
	implicit def sequencedUnitGeneric[B]:Sequenced[Unit, B, B] = apply((_:Unit, b:B) => b)
	implicit def sequencedGenericUnit[A]:Sequenced[A, Unit, A] = apply((a:A, _:Unit) => a)
}

private[typelevel] trait LowPrioSequenced {
	implicit def sequencedGenricToPair[A, B]:Sequenced[A, B, (A, B)] = Sequenced.apply((a:A, b:B) => (a, b))
}
