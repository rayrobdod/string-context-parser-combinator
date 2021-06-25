package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class Opaque[Expr, A](
	backing:Parser[Expr, A],
	description:ExpectingDescription
) extends AbstractParser[Expr, A] {
	def parse(input:Input[Expr]):Result[Expr, A] = {
		backing.parse(input) match {
			case success:Success[Expr, A] => success.map({case Success1(value, rest, _, cut) => Success1(value, rest, Set.empty, cut)})
			case Failure(_, cut) => Failure(Expecting(description, input.position), cut)
		}
	}
}
