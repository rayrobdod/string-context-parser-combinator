package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class Opaque[Expr, A](
	backing:Parser[Expr, A],
	description:ExpectingDescription
) extends Parser[Expr, A] {
	def parse[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
		val descriptionPosition = ExpectingSet(Expecting(description, input.position))
		backing.parse(input) match {
			case success:Success[ExprZ, Pos, A] => success.map({case Success1(value, rest, _, cut) => Success1(value, rest, descriptionPosition, cut)})
			case Failure(_, cut) => Failure(descriptionPosition, cut)
		}
	}
}
