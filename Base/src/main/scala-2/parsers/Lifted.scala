package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.language.higherKinds
import scala.reflect.macros.blackbox.Context

private[stringContextParserCombinator]
object Lifted {
	def apply[Lifter[A], Z](
		c:Context)(
		lift:LiftFunction[c.type, Lifter, Z],
		description:ExpectingDescription
		)(implicit lifterTypeTag:c.TypeTag[Lifter[_]]
	):AbstractParser[c.Expr[_], Z] = {
		new AbstractParser[c.Expr[_], Z] {
			override def parse(input:Input[c.Expr[_]]):Result[c.Expr[_], Z] = {
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
							Option(lift.apply(c.Expr(lifterTree)(c.TypeTag(lifterType)), liftee))
						}
					},
					description
				)
			}
		}
	}
}
