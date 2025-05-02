package name.rayrobdod.stringContextParserCombinator
package internal

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

/** Succeeds if the next input element is an `arg` with the given type; captures the expression */
private[stringContextParserCombinator]
final class OfType[A](using typeCreator:TypeCreator[A]) extends Parser[Quotes, Expr, TypeCreator, Expr[A]] {
	override def interpolate[ExprZ <: Expr[_], Pos](input:Input[ExprZ, Pos])(implicit quotes:Quotes, ev1:Ordering[Pos]):Result[ExprZ, Pos, Expr[A]] = {
		given Type[A] = typeCreator.createType
		input.consume(
			_ => None,
			arg => Some(arg).collect({case x if x.isExprOf[A] => x.asExprOf[A]}),
			ExpectingDescription(s"OfType(${Type.show[A]})"),
		)
	}

	override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Quotes, ev1:Ordering[Pos], exprs:UnapplyExprs[Quotes, Expr, TypeCreator]):Result[Unit, Pos, UnapplyExpr[Quotes, Expr, TypeCreator, Expr[A]]] = {
		given Type[A] = typeCreator.createType
		input.consume(
			_ => None,
			(_:Unit) => Some(exprs.ofType(typeCreator)),
			ExpectingDescription(s"OfType(${Type.show[A]})"),
		)
	}
}
