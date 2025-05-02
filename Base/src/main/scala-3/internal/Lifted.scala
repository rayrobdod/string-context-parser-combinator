package name.rayrobdod.stringContextParserCombinator
package internal

import scala.annotation.nowarn
import scala.language.higherKinds
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

private[stringContextParserCombinator]
object Lifted {
	/* Help the type checker by asserting here that `Expr[A]` and `Expr[Lifter[A]]` use the same `A` */
	private final class MyTupleOpt[A : Type, Lifter[_]](value:Expr[A], lifter:Option[Expr[Lifter[A]]]) {
		def transpose:Option[MyTuple[A, Lifter]] = lifter.map(x => MyTuple(value, x))
	}
	private final class MyTuple[A : Type, Lifter[_]](value:Expr[A], lifter:Expr[Lifter[A]]) {
		def liftApply[Z](lift:LiftFunction[Lifter, Z])(using Quotes):Z = lift.apply[A](lifter, value)
	}

	def apply[Lifter[_], Z](
		lift:LiftFunction[Lifter, Z],
		description:ExpectingDescription,
		)(using
		TypeCreator[Lifter],
	):Interpolator[Quotes, Expr[_], Z] = {
		new Interpolator[Quotes, Expr[_], Z] {
			def interpolate[ExprZ <: Expr[_], Pos](input:Input[ExprZ, Pos])(implicit quotes: Quotes, ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				@nowarn("id=E198") given Type[Lifter] = TypeCreator[Lifter].createType
				input.consume(
					_ => None,
					arg => (Some(arg)
						.collect({case '{ $x: t } => new MyTupleOpt[t, Lifter](x, Expr.summon[Lifter[t]])})
						.flatMap(_.transpose)
						.map(_.liftApply[Z](lift))
					),
					description
				)
			}
		}
	}
}
