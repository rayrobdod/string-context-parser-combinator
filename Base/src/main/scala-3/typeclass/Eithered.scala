package com.rayrobdod.stringContextParserCombinator
package typeclass

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
	def apply[A, B, Z](leftFn:A => Z, rightFn:B => Z):Eithered[A, B, Z] = {
		final class Apply extends Eithered[A, B, Z] {
			def left(elem:A):Z = leftFn(elem)
			def right(elem:B):Z = rightFn(elem)
		}
		new Apply()
	}

	given unitUnit:Eithered[Unit, Unit, Unit] = generic[Unit, Unit]
	given unitAny[B, Z](using ev:Optionally[B, Z]):Eithered[Unit, B, Z] = Eithered(_ => ev.none, ev.some _)
	given anyUnit[A, Z](using ev:Optionally[A, Z]):Eithered[A, Unit, Z] = Eithered(ev.some _, _ => ev.none)

	def discriminatedUnion[A, B]:Eithered[A, B, Either[A, B]] = Eithered(Left.apply _, Right.apply _)
}

private[typeclass] trait LowPrioEithered {
	given generic[A, B]:Eithered[A, B, A | B] = Eithered.apply(Predef.identity _, Predef.identity _)
}
