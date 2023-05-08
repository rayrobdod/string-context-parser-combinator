package com.rayrobdod.stringContextParserCombinator
package typeclass

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

/**
 * Describes how to separate a value into two adjacent values
 *
 * @tparam A the first result
 * @tparam B the second result
 * @tparam Z the input value
 */
trait ContraSequenced[+A, +B, -Z] {
	def separate(value:Z):(A, B)
}

trait BiSequenced[A, B, Z]
		extends Sequenced[A, B, Z]
		with ContraSequenced[A, B, Z]

/** Predefined implicit implementations of Sequenced */
object Sequenced extends LowPrioSequenced {
	implicit def unitUnit:Sequenced[Unit, Unit, Unit] = BiSequenced.unitUnit
	implicit def unitGeneric[B]:Sequenced[Unit, B, B] = BiSequenced.unitGeneric
	implicit def genericUnit[A]:Sequenced[A, Unit, A] = BiSequenced.genericUnit
}

private[typeclass] trait LowPrioSequenced {
	implicit def toPair[A, B]:Sequenced[A, B, (A, B)] = BiSequenced.toPair
}

/** Predefined implicit implementations of ContraSequenced */
object ContraSequenced extends LowPrioContraSequenced {
	implicit def unitUnit:ContraSequenced[Unit, Unit, Unit] = BiSequenced.unitUnit
	implicit def unitGeneric[B]:ContraSequenced[Unit, B, B] = BiSequenced.unitGeneric
	implicit def genericUnit[A]:ContraSequenced[A, Unit, A] = BiSequenced.genericUnit
}

private[typeclass] trait LowPrioContraSequenced {
	implicit def toPair[A, B]:ContraSequenced[A, B, (A, B)] = BiSequenced.toPair
}

/** Predefined implicit implementations of BiSequenced */
object BiSequenced extends LowPrioBiSequenced {
	private[typeclass] def apply[A, B, Z](aggregateFn:(A, B) => Z, separateFn:Z => (A, B)):BiSequenced[A, B, Z] = {
		final class Apply extends BiSequenced[A, B, Z] {
			def aggregate(left:A, right:B):Z = aggregateFn(left, right)
			def separate(value:Z):(A, B) = separateFn(value)
		}
		new Apply()
	}

	implicit def unitUnit:BiSequenced[Unit, Unit, Unit] = apply((_:Unit, _:Unit) => (), (_:Unit) => ((), ()))
	implicit def unitGeneric[B]:BiSequenced[Unit, B, B] = apply((_:Unit, b:B) => b, (b:B) => ((), b))
	implicit def genericUnit[A]:BiSequenced[A, Unit, A] = apply((a:A, _:Unit) => a, (a:A) => (a, ()))
}

private[typeclass] trait LowPrioBiSequenced {
	implicit def toPair[A, B]:BiSequenced[A, B, (A, B)] = BiSequenced.apply((a:A, b:B) => (a, b), (ab:(A,B)) => (ab._1, ab._2))
}
