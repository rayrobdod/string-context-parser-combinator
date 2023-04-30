package com.rayrobdod.stringContextParserCombinator
package typeclass

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

/**
 * Describes how to disambiguate the union of two types
 *
 * The parser determines whether the some or none branch is taken.
 * The return value's `Expr[Boolean]` indicates whether the value matches the branch
 */
trait ContraOptionally[+Expr[_], +A, -Z] {
	def contraNone(elem:Z):Expr[Boolean]
	def contraSome:PartialExprFunction[Expr, Z, A]
}

trait BiOptionally[Expr[_], A, Z]
	extends Optionally[A, Z]
	with ContraOptionally[Expr, A, Z]

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

	implicit def unit:Optionally[Unit, Unit] = this.whereDefault(())
}

private[typeclass] trait LowPrioOptionally {
	implicit def toOption[A]:Optionally[A, Option[A]] = Optionally(None, Some.apply _)
}

object ContraOptionally extends VersionSpecificContraOptionally with LowPrioContraOptionally {
	def apply[Expr[_], A, Z](
		contraNoneFn:Z => Expr[Boolean],
		contraSomeFn:PartialExprFunction[Expr, Z, A]
	):ContraOptionally[Expr, A, Z] = {
		final class Apply extends ContraOptionally[Expr, A, Z] {
			override def contraNone(elem:Z):Expr[Boolean] = contraNoneFn(elem)
			override def contraSome:PartialExprFunction[Expr, Z, A] = contraSomeFn
		}
		new Apply
	}

	implicit def idUnit:ContraOptionally[Id, Unit, Unit] = BiOptionally.idUnit
}

private[typeclass] trait LowPrioContraOptionally {
	implicit def idToOption[A]:ContraOptionally[Id, A, Option[A]] = BiOptionally.idToOption
}

object BiOptionally extends VersionSpecificBiOptionally with LowPrioBiOptionally {
	def apply[Expr[_], A, Z](
		noneFn: => Z,
		someFn:A => Z,
		contraNoneFn:Z => Expr[Boolean],
		contraSomeFn:PartialExprFunction[Expr, Z, A]
	):BiOptionally[Expr, A, Z] = {
		final class Apply extends BiOptionally[Expr, A, Z] {
			override def none:Z = noneFn
			override def some(elem:A):Z = someFn(elem)
			override def contraNone(elem:Z):Expr[Boolean] = contraNoneFn(elem)
			override def contraSome:PartialExprFunction[Expr, Z, A] = contraSomeFn
		}
		new Apply
	}

	implicit def idUnit:BiOptionally[Id, Unit, Unit] = BiOptionally.apply[Id, Unit, Unit](
		(),
		_ => (),
		_ => true,
		PartialExprFunction.identity[Id, Unit](true)
	)
}

private[typeclass] trait LowPrioBiOptionally extends VersionSpecificLowPrioBiOptionally {
	implicit def idToOption[A]:BiOptionally[Id, A, Option[A]] = BiOptionally.apply[Id, A, Option[A]](
		None,
		Some.apply _,
		_.isEmpty,
		PartialExprFunction[Id, Option[A], A](
			_.nonEmpty,
			_.get
		)
	)
}
