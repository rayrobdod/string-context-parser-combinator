package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class Map[Expr, A, Z](
	backing:Parser[Expr,A], mapping:Function1[A, Z]
) extends AbstractParser[Expr, Z] {
	def parse(input:Input[Expr]):Result[Expr, Z] = {
		backing.parse(input) match {
			case success:Success[Expr, A] => success.map(mapping)
			case failure:Failure => failure
		}
	}
}
