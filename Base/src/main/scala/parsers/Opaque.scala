package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class Opaque[Expr, A](
	backing:Parser[Expr, A],
	description:ExpectingDescription
) extends AbstractParser[Expr, A] {
	def parse(input:Input[Expr]):Result[Expr, A] = {
		val trace = LeafTrace(description, input)
		backing.parse(input) match {
			case Success(value, rest, _, cut) => Success(value, rest, trace, cut)
			case Failure(_, cut) => Failure(trace, cut)
		}
	}
}
