package com.rayrobdod.stringContextParserCombinator
package typelevel

/**
 * Describes how to represent the union of two types
 *
 * @tparam A the first choice
 * @tparam B the second choice
 * @tparam Z the result container
 */
trait Eithered[-A, -B, +Z] {
	def left(elem:A):Z
	def right(elem:B):Z
}

/** Predefined implicit implementations of Eithered */
object Eithered extends LowPrioEithered {
	private[typelevel] def apply[A, B, Z](leftFn:A => Z, rightFn:B => Z):Eithered[A, B, Z] = {
		final class Apply extends Eithered[A, B, Z] {
			def left(elem:A):Z = leftFn(elem)
			def right(elem:B):Z = rightFn(elem)
		}
		new Apply()
	}

	implicit def eitheredUnitUnit:Eithered[Unit, Unit, Unit] = eitheredSymetric[Unit]
	implicit def eitheredUnitGeneric[B, Z](implicit ev:Optionally[B, Z]):Eithered[Unit, B, Z] = Eithered(_ => ev.none, ev.some _)
	implicit def eitheredGenericUnit[A, Z](implicit ev:Optionally[A, Z]):Eithered[A, Unit, Z] = Eithered(ev.some _, _ => ev.none)

	def discriminatedUnion[A, B]:Eithered[A, B, Either[A, B]] = Eithered(Left.apply _, Right.apply _)
}

private[typelevel] trait LowPrioEithered {
	implicit def eitheredSymetric[A]:Eithered[A, A, A] = Eithered(Predef.identity _, Predef.identity _)
}
