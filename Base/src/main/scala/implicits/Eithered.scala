package com.rayrobdod.stringContextParserCombinator
package typelevel

// TODO: use scala 3's union type `A | B`

/**
 * Describes how to represent the union of two types
 */
trait Eithered[-A, -B, +Z] {
	def left(elem:A):Z
	def right(elem:B):Z
}

/** Predefined implicit implementations of Eithered */
object Eithered extends LowPrioEithered {
	implicit def eitheredUnitUnit:Eithered[Unit, Unit, Unit] = eitheredSymetric[Unit]

	implicit def eitheredUnitGeneric[B, Z](implicit ev:Optionally[B, Z]):Eithered[Unit, B, Z] = new EitheredUnitGeneric[B, Z](ev)
	private[this] final class EitheredUnitGeneric[B, Z](ev:Optionally[B, Z]) extends Eithered[Unit, B, Z] {
		def left(elem:Unit):Z = ev.none
		def right(elem:B):Z = ev.some(elem)
	}
	implicit def eitheredGenericUnit[A, Z](implicit ev:Optionally[A, Z]):Eithered[A, Unit, Z] = new EitheredGenericUnit[A, Z](ev)
	private[this] final class EitheredGenericUnit[A, Z](ev:Optionally[A, Z]) extends Eithered[A, Unit, Z] {
		def left(elem:A):Z = ev.some(elem)
		def right(elem:Unit):Z = ev.none
	}

	def discriminatedUnion[A, B]:Eithered[A, B, Either[A, B]] = new GenericToEither[A, B]
	private[this] final class GenericToEither[A, B] extends Eithered[A, B, Either[A, B]] {
		def left(elem:A):Either[A, B] = Left(elem)
		def right(elem:B):Either[A, B] = Right(elem)
	}
}

private[typelevel] trait LowPrioEithered {
	implicit def eitheredSymetric[A]:Eithered[A, A, A] = new EitheredSymetric[A]
	private[this] final class EitheredSymetric[A] extends Eithered[A, A, A] {
		def left(elem:A):A = elem
		def right(elem:A):A = elem
	}
}
