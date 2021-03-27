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
			case Success(_, _, _, cut) => {
				backing.parse(new Input[Nothing](List(("", input.position)), List.empty, x => x)) match {
					case Success(_, _, _, _) => Failure(Set(Expecting(s"??? where ${predicateDescription.value}", input.position)), cut)
					case Failure(t, _) => Failure(t.map(_.where(predicateDescription)), cut)
				}
			}
			case failure => failure
		}
	}
}
