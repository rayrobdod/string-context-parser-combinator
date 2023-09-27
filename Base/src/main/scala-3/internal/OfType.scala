package name.rayrobdod.stringContextParserCombinator
package internal

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

/** Succeeds if the next input element is an `arg` with the given type; captures the expression */
private[stringContextParserCombinator]
final class OfType[A](using Type[A], Quotes) extends Parser[Expr, Type, Expr[A]] {
	private val expecting = ExpectingDescription(s"OfType(${Type.show[A]})")

	def interpolate[ExprZ <: Expr[_], Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Expr[A]] = {
		input.consume(
			_ => None,
			arg => Some(arg).collect({case x if x.isExprOf[A] => x.asExprOf[A]}),
			expecting,
		)
	}

	override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Expr[A]]] = {
		input.consume(
			_ => None,
			(_:Unit) => Some(UnapplyExpr.OfType(implicitly[Type[A]])),
			expecting
		)
	}
}
