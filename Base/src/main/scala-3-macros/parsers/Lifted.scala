package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.language.higherKinds
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

private[stringContextParserCombinator]
object Lifted {
	/* Help the type checker by asserting here that `Expr[A]` and `Expr[Lifter[A]]` use the same `A` */
	private final class MyTupleOpt[A, Lifter[A]](value:Expr[A], lifter:Option[Expr[Lifter[A]]])(using Type[A]) {
		def transpose:Option[MyTuple[A, Lifter]] = lifter.map(x => MyTuple(value, x))
	}
	private final class MyTuple[A, Lifter[A]](value:Expr[A], lifter:Expr[Lifter[A]])(using Type[A]) {
		def liftApply[Z](lift:LiftFunction[Lifter, Z])(using Quotes):Expr[Z] = lift.apply[A](lifter, value)
	}

	def apply[Lifter[A], Z](
		lift:LiftFunction[Lifter, Z],
		description:Expecting,
		)(using
		Quotes,
		Type[Lifter],
	):AbstractParser[Expr[_], Expr[Z]] = {
		new AbstractParser[Expr[_], Expr[Z]] {
			def parse(input:Input[Expr[_]]):Result[Expr[_], Expr[Z]] = {
				input.consume(
					_ => None,
					arg => (Some(arg)
						.collect({case '{ $x: t } => new MyTupleOpt(x, Expr.summon[Lifter[t]])})
						.flatMap(_.transpose)
						.map(_.liftApply[Z](lift))
					),
					description
				)
			}
		}
	}
}
