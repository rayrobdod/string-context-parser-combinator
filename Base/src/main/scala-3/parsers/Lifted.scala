package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.language.higherKinds
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

private[stringContextParserCombinator]
object Lifted {
	/* Help the type checker by asserting here that `Expr[A]` and `Expr[Lifter[A]]` use the same `A` */
	private final class MyTupleOpt[A : Type, Lifter[A]](value:Expr[A], lifter:Option[Expr[Lifter[A]]]) {
		def transpose:Option[MyTuple[A, Lifter]] = lifter.map(x => MyTuple(value, x))
	}
	private final class MyTuple[A : Type, Lifter[A]](value:Expr[A], lifter:Expr[Lifter[A]]) {
		def liftApply[Z](lift:LiftFunction[Lifter, Z])(using Quotes):Z = lift.apply[A](lifter, value)
	}

	def apply[Lifter[A] : Type, Z](
		lift:LiftFunction[Lifter, Z],
		description:ExpectingDescription,
		)(using
		Quotes,
	):AbstractParser[Expr[_], Z] = {
		new AbstractParser[Expr[_], Z] {
			def parse[Pos](input:Input[Expr[_], Pos]):Result[Expr[_], Pos, Z] = {
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
