package com.rayrobdod.stringContextParserCombinator
package parsers

private[parsers] final class Map[Expr, A, Z](
	backing:Parser[Expr,A], mapping:Function1[A, Z]
) extends AbstractParser[Expr, Z] {
	def parse(input:Input[Expr]):Result[Expr, Z] = {
		backing.parse(input) match {
			case Success(value, rest, trace) => Success(mapping(value), rest, trace)
			case failure@Failure(_) => failure
		}
	}
}
