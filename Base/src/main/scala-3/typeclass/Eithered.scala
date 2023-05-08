package com.rayrobdod.stringContextParserCombinator
package typeclass

import scala.quoted.*

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

/**
 * Describes how to disambiguate the union of two types
 *
 * The parser determines whether the left or right branch is taken.
 * The return value's `Expr[Boolean]` indicates whether the value matches the branch
 */
trait ContraEithered[+Expr[_], +A, +B, -Z] {
	def contraLeft:PartialExprFunction[Expr, Z, A]
	def contraRight:PartialExprFunction[Expr, Z, B]
}

trait BiEithered[Expr[_], A, B, Z]
		extends Eithered[A, B, Z]
		with ContraEithered[Expr, A, B, Z]

/** Predefined implicit implementations of Eithered */
object Eithered extends LowPrioEithered {
	def apply[A, B, Z](leftFn:A => Z, rightFn:B => Z):Eithered[A, B, Z] = {
		final class Apply extends Eithered[A, B, Z] {
			def left(elem:A):Z = leftFn(elem)
			def right(elem:B):Z = rightFn(elem)
		}
		new Apply()
	}

	given unitUnit:Eithered[Unit, Unit, Unit] = Eithered.generic
	given unitAny[B, Z](using ev:Optionally[B, Z]):Eithered[Unit, B, Z] = Eithered(_ => ev.none, ev.some _)
	given anyUnit[A, Z](using ev:Optionally[A, Z]):Eithered[A, Unit, Z] = Eithered(ev.some _, _ => ev.none)

	def discriminatedUnion[A, B]:Eithered[A, B, Either[A, B]] = Eithered(Left.apply, Right.apply)
}

private[typeclass] trait LowPrioEithered {
	given generic[A, B]:Eithered[A, B, A | B] = Eithered.apply(Predef.identity _, Predef.identity _)
}

/** Predefined implicit implementations of ContraEithered */
object ContraEithered extends LowPrioContraEithered {
	def apply[Expr[_], A, B, Z](
		contraLeftFn:PartialExprFunction[Expr, Z, A],
		contraRightFn:PartialExprFunction[Expr, Z, B],
	):ContraEithered[Expr, A, B, Z] = {
		final class Apply extends ContraEithered[Expr, A, B, Z] {
			override def contraLeft:PartialExprFunction[Expr, Z, A] = contraLeftFn
			override def contraRight:PartialExprFunction[Expr, Z, B] = contraRightFn
		}
		new Apply()
	}

	given quotedUnitUnit(using Quotes):ContraEithered[Expr, Unit, Unit, Unit] = quotedSymmetric[Unit]

	given idUnitUnit:ContraEithered[Id, Unit, Unit, Unit] = idSymmetric[Unit]
}

private[typeclass] trait LowPrioContraEithered {
	given quotedSymmetric[A](using Quotes):ContraEithered[Expr, A, A, A] = BiEithered.quotedSymmetric
	given idSymmetric[A]:ContraEithered[Id, A, A, A] = BiEithered.idSymmetric
}

/** Predefined implicit implementations of BiEithered */
object BiEithered extends LowPrioBiEithered {
	def apply[Expr[_], A, B, Z](
		leftFn:A => Z,
		rightFn:B => Z,
		contraLeftFn:PartialExprFunction[Expr, Z, A],
		contraRightFn:PartialExprFunction[Expr, Z, B],
	):BiEithered[Expr, A, B, Z] = {
		final class Apply extends BiEithered[Expr, A, B, Z] {
			override def left(elem:A):Z = leftFn(elem)
			override def right(elem:B):Z = rightFn(elem)

			override def contraLeft:PartialExprFunction[Expr, Z, A] = contraLeftFn
			override def contraRight:PartialExprFunction[Expr, Z, B] = contraRightFn
		}
		new Apply()
	}

	given quotedUnitUnit(using Quotes):BiEithered[Expr, Unit, Unit, Unit] = quotedSymmetric[Unit]

	given eitherUnitAny[Expr[_], B, Z](using ev:BiOptionally[Expr, B, Z])(using Quotes):BiEithered[Expr, Unit, B, Z] = BiEithered(
		_ => ev.none,
		ev.some _,
		PartialExprFunction[Expr, Z, Unit](
			ev.contraNone,
			_ => ()
		),
		ev.contraSome,
	)
	given eitherAnyUnit[Expr[_], A, Z](using ev:BiOptionally[Expr, A, Z])(using Quotes):BiEithered[Expr, A, Unit, Z] = BiEithered(
		ev.some _,
		_ => ev.none,
		ev.contraSome,
		PartialExprFunction[Expr, Z, Unit](
			ev.contraNone,
			_ => ()
		),
	)

	given idUnitUnit:BiEithered[Id, Unit, Unit, Unit] = idSymmetric[Unit]
}

private[typeclass] trait LowPrioBiEithered {
	given quotedSymmetric[A](using Quotes):BiEithered[Expr, A, A, A] = BiEithered.apply(
		Predef.identity _,
		Predef.identity _,
		PartialExprFunction.identity(Expr(true)),
		PartialExprFunction.identity(Expr(true)),
	)

	given idSymmetric[A]:BiEithered[Id, A, A, A] = BiEithered.apply(
		Predef.identity _,
		Predef.identity _,
		PartialExprFunction.identity[Id, A](true),
		PartialExprFunction.identity[Id, A](true),
	)
}
