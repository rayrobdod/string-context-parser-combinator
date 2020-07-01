package com.rayrobdod.stringContextParserCombinator
package parsers

private[parsers] final class Opaque[Expr, A](
	backing:Parser[Expr, A], description:Failure.Expecting
) extends AbstractParser[Expr, A] {
	def parse(input:Input[Expr]):Result[Expr, A] = {
		backing.parse(input) match {
			case Success(v, r) => Success(v,r)
			case Failure(_, rest) => Failure(description, rest)
		}
	}
}
