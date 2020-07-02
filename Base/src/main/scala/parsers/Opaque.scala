package com.rayrobdod.stringContextParserCombinator
package parsers

private[parsers] final class Opaque[Expr, A](
	backing:Parser[Expr, A],
	description:Expecting
) extends AbstractParser[Expr, A] {
	def parse(input:Input[Expr]):Result[Expr, A] = {
		val trace = LeafTrace(description, input)
		backing.parse(input) match {
			case Success(value, rest, _) => Success(value, rest, trace)
			case Failure(_) => Failure(trace)
		}
	}
}
