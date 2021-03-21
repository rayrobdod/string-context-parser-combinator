package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class Filter[Expr, A](
	backing:Parser[Expr, A],
	predicate:Function1[A, Boolean],
	predicateDescription:ExpectingDescription
) extends AbstractParser[Expr, A] {
	def parse(input:Input[Expr]):Result[Expr, A] = {
		backing.parse(input) match {
			case success@Success(value, _, _, _) if predicate(value) => success
			case Success(_, _, trace, cut) => Failure(FilterTrace(predicateDescription, trace), cut)
			case failure => failure
		}
	}
}
