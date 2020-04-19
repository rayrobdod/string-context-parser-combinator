package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.language.higherKinds
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[stringContextParserCombinator]
object Lifted {
	def apply[Lifter[A], Z](
		c:Context)(
		lifterType:Function1[c.Type, c.Type],
		lift:LiftFunction[c.type, Lifter, Z],
		description:Failure.Expecting
	):AbstractParser[c.type, c.Expr[Z]] = {
		new AbstractParser[c.type, c.Expr[Z]] {
			def parse(input:Input[c.type]):Result[c.type, c.Expr[Z]] = {
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
