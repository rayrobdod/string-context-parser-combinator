package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

private[parsers]
final class OfType[A](
	tpetag:Type[A],
)(	using Quotes,
) extends AbstractParser[Expr[_], Expr[A]] {
	def parse(input:Input[Expr[_]]):Result[Expr[_], Expr[A]] = {
		input.consume(
			_ => None,
			arg => Some(arg).collect({case x if x.isExprOf(using tpetag) => x.asExprOf[A](using tpetag)}),
			Expecting(Type.show(using tpetag))
		)
	}
}

private[stringContextParserCombinator]
object OfType {
	/** Succeeds if the next input element is an `arg` with the given type; captures the expression */
	def apply[A](
		tpetag:Type[A],
	)(	using Quotes,
	):Parser[Expr[_], Expr[A]] = {
		new OfType(tpetag)
	}
}
