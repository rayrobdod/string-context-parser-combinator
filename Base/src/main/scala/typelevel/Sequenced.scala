package com.rayrobdod.stringContextParserCombinator
package typelevel

/**
 * Describes how to combine two adjacent values into one value
 *
 * @tparam A the first input
 * @tparam B the second input
 * @tparam Z the result container
 */
trait Sequenced[-A, -B, +Z] {
	def aggregate(left:A, right:B):Z
}

/** Predefined implicit implementations of Sequenced */
object Sequenced extends LowPrioSequenced {
	implicit def sequencedUnitUnit:Sequenced[Unit, Unit, Unit] = new SequencedUnitUnit
	private[this] final class SequencedUnitUnit extends Sequenced[Unit, Unit, Unit] {
		def aggregate(a:Unit, b:Unit):Unit = ()
	}
	implicit def sequencedUnitGeneric[B]:Sequenced[Unit, B, B] = new SequencedUnitGeneric
	private[this] final class SequencedUnitGeneric[B] extends Sequenced[Unit, B, B] {
		def aggregate(u:Unit, b:B):B = b
	}
	implicit def sequencedGenericUnit[A]:Sequenced[A, Unit, A] = new SequencedGenericUnit
	private[this] final class SequencedGenericUnit[A] extends Sequenced[A, Unit, A] {
		def aggregate(a:A, u:Unit):A = a
	}
}

private[typelevel] trait LowPrioSequenced {
	implicit def sequencedGenricToPair[A,B]:Sequenced[A, B, (A, B)] = new SequencedGenricToPair
	private[this] final class SequencedGenricToPair[A, B] extends Sequenced[A, B, (A, B)] {
		def aggregate(a:A, b:B):(A,B) = (a, b)
	}
}
