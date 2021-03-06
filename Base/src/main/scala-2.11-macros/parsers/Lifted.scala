package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.language.higherKinds
import scala.reflect.macros.blackbox.Context

private[stringContextParserCombinator]
object Lifted {
	def apply[Lifter[A], Z](
		c:Context)(
		lifterType:Function1[c.Type, c.Type],
		lift:LiftFunction[c.type, Lifter, Z],
		description:ExpectingDescription
	):AbstractParser[c.Expr[_], Z] = {
		new AbstractParser[c.Expr[_], Z] {
			override def parse(input:Input[c.Expr[_]]):Result[c.Expr[_], Z] = {
				input.consume(
					_ => None,
					arg => (Some(arg)
						.map(x => ((c.inferImplicitValue(lifterType(x.actualType)), x.tree)))
						.filter(! _._1.isEmpty)
						.map(x => lift.apply(c.Expr(x._1), c.Expr(x._2)))
					),
					description
				)
			}
		}
	}
}
