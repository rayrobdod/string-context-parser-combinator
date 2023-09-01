package name.rayrobdod.stringContextParserCombinator
package internal

import scala.reflect.macros.blackbox.Context

private[stringContextParserCombinator]
object Lifted {
	def apply[Lifter[_], Z](
		c:Context)(
		lift:LiftFunction[c.type, Lifter, Z],
		description:ExpectingDescription
		)(implicit lifterTypeTag:c.TypeTag[Lifter[_]]
	):Interpolator[c.Expr[_], Z] = {
		new Interpolator[c.Expr[_], Z] {
			override def interpolate[ExprZ <: c.Expr[_], Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				input.consume(
					_ => None,
					liftee => {
						val lifteeType = liftee.actualType
						val lifterTypeConstructor = lifterTypeTag.tpe.typeConstructor
						val lifterType = c.universe.appliedType(lifterTypeConstructor, List(lifteeType))
						val lifterTree = c.inferImplicitValue(lifterType)
						if (lifterTree.isEmpty) {
							None
						} else {
							Option(lift.apply(c.Expr(lifterTree)(c.TypeTag(lifterType)), liftee:c.Expr[_]))
						}
					},
					description
				)
			}
		}
	}
}
