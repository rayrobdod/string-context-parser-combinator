package com.rayrobdod.stringContextParserCombinator
package parsers

private[parsers] final class Filter[Expr, A](
	backing:Parser[Expr, A], predicate:Function1[A, Boolean], val predicateDescription:Expecting
) extends AbstractParser[Expr, A] {
	def parse(input:Input[Expr]):Result[Expr, A] = {
		backing.parse(input) match {
			case success@Success(value, _, _) if predicate(value) => success
			case Success(_, _, trace) => Failure(FilterTrace(predicateDescription, trace))
			case failure => failure
		}
	}
}
