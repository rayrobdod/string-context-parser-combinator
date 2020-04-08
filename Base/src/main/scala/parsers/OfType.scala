package com.rayrobdod.stringContextParserCombinator
package parsers

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers]
final class OfType[U <: Context with Singleton, A](
	tpetag:U#TypeTag[A]
) extends AbstractParser[U, U#Expr[A]] {
	def parse(input:Input[U]):Result[U, U#Expr[A]] = {
		input.consume(
			_ => None,
			arg => Some(arg).filter(x => x.actualType <:< tpetag.tpe).map(_.asInstanceOf[U#Expr[A]]),
			Failure.Leaf(tpetag.tpe.toString)
		)
	}
}
