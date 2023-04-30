package com.rayrobdod.stringContextParserCombinator
package typeclass

import scala.reflect.macros.blackbox.Context

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

	implicit def unitUnit:Eithered[Unit, Unit, Unit] = symmetric[Unit]
	implicit def unitGeneric[B, Z](implicit ev:Optionally[B, Z]):Eithered[Unit, B, Z] = Eithered(_ => ev.none, ev.some _)
	implicit def genericUnit[A, Z](implicit ev:Optionally[A, Z]):Eithered[A, Unit, Z] = Eithered(ev.some _, _ => ev.none)

	def discriminatedUnion[A, B]:Eithered[A, B, Either[A, B]] = Eithered(Left.apply _, Right.apply _)
}

private[typeclass] trait LowPrioEithered {
	implicit def symmetric[A]:Eithered[A, A, A] = Eithered(Predef.identity _, Predef.identity _)
}

/** Predefined implicit implementations of ContraEithered */
object ContraEithered extends LowPrioContraEithered {
	def apply[Expr[_], A, B, Z](
		contraLeftFn:PartialExprFunction[Expr, Z, A],
		contraRightFn:PartialExprFunction[Expr, Z, B]
	):ContraEithered[Expr, A, B, Z] = {
		final class Apply extends ContraEithered[Expr, A, B, Z] {
			override def contraLeft:PartialExprFunction[Expr, Z, A] = contraLeftFn
			override def contraRight:PartialExprFunction[Expr, Z, B] = contraRightFn
		}
		new Apply()
	}

	trait ContraEithereds[Expr[_]] extends LowPrioContraEithereds[Expr] {
		implicit def unitUnit:ContraEithered[Expr, Unit, Unit, Unit]
	}
	private[typeclass]
	trait LowPrioContraEithereds[Expr[_]] {
		implicit def symmetric[A]:ContraEithered[Expr, A, A, A]
	}

	def forContext(c:Context):ContraEithereds[c.Expr] = {
		val backing = BiEithered.forContext(c)

		new ContraEithereds[c.Expr] {
			implicit override def unitUnit:ContraEithered[c.Expr, Unit, Unit, Unit] = backing.unitUnit
			implicit override def symmetric[A]:ContraEithered[c.Expr, A, A, A] = backing.symmetric[A]
		}
	}
}

private[typeclass] trait LowPrioContraEithered {
	implicit def idSymmetric[A]:ContraEithered[Id, A, A, A] = BiEithered.idSymmetric
}

/** Predefined implicit implementations of BiEithered */
object BiEithered extends LowPrioBiEithered {
	def apply[Expr[_], A, B, Z](
		leftFn:A => Z,
		rightFn:B => Z,
		contraLeftFn:PartialExprFunction[Expr, Z, A],
		contraRightFn:PartialExprFunction[Expr, Z, B]
	):BiEithered[Expr, A, B, Z] = {
		final class Apply extends BiEithered[Expr, A, B, Z] {
			override def left(elem:A):Z = leftFn(elem)
			override def right(elem:B):Z = rightFn(elem)

			override def contraLeft:PartialExprFunction[Expr, Z, A] = contraLeftFn
			override def contraRight:PartialExprFunction[Expr, Z, B] = contraRightFn
		}
		new Apply()
	}

	trait BiEithereds[Expr[_]] extends LowPrioBiEithereds[Expr] {
		implicit def unitUnit:BiEithered[Expr, Unit, Unit, Unit]
	}
	private[typeclass]
	trait LowPrioBiEithereds[Expr[_]] {
		implicit def symmetric[A]:BiEithered[Expr, A, A, A]
	}

	def forContext(c:Context):BiEithereds[c.Expr] = {
		new BiEithereds[c.Expr] {
			override def unitUnit:BiEithered[c.Expr, Unit, Unit, Unit] = this.symmetric[Unit]

			implicit override def symmetric[A]:BiEithered[c.Expr, A, A, A] = {
				val exprTrue = c.Expr[Boolean](c.universe.Liftable.liftBoolean(true))

				BiEithered.apply[c.Expr, A, A, A](
					Predef.identity _,
					Predef.identity _,
					PartialExprFunction.identity(exprTrue),
					PartialExprFunction.identity(exprTrue)
				)
			}
		}
	}
}

private[typeclass] trait LowPrioBiEithered {
	implicit def idSymmetric[A]:BiEithered[Id, A, A, A] = {
		BiEithered.apply[Id, A, A, A](
			Predef.identity _,
			Predef.identity _,
			PartialExprFunction.identity[Id, A](true),
			PartialExprFunction.identity[Id, A](true)
		)
	}
}
