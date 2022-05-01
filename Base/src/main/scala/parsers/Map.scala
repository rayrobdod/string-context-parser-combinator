package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class Map[Expr, A, Z](
	backing:Parser[Expr,A], mapping:Function1[A, Z]
) extends AbstractParser[Expr, Z] {
	def parse[Pos](input:Input[Expr, Pos]):Result[Expr, Pos, Z] = {
		backing.parse(input) match {
			case success:Success[Expr, Pos, A] => success.mapValues(mapping)
			case failure:Failure[Pos] => failure
		}
	}
}
