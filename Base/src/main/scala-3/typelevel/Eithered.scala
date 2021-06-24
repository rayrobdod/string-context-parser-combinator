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
	given unitUnit:Eithered[Unit, Unit, Unit] = generic[Unit, Unit]

	given unitAny[B, Z](using ev:Optionally[B, Z]):Eithered[Unit, B, Z] with {
		def left(elem:Unit):Z = ev.none
		def right(elem:B):Z = ev.some(elem)
	}
	given anyUnit[A, Z](using ev:Optionally[A, Z]):Eithered[A, Unit, Z] with {
		def left(elem:A):Z = ev.some(elem)
		def right(elem:Unit):Z = ev.none
	}

	def discriminatedUnion[A, B]:Eithered[A, B, Either[A, B]] = new DiscriminatedUnion[A, B]
	private[this] final class DiscriminatedUnion[A, B] extends Eithered[A, B, Either[A, B]] {
		def left(elem:A):Either[A, B] = Left(elem)
		def right(elem:B):Either[A, B] = Right(elem)
	}
}

private[typelevel] trait LowPrioEithered {
	given generic[A, B]:Eithered[A, B, A | B] with {
		def left(elem:A):A | B = elem
		def right(elem:B):A | B = elem
	}
}
