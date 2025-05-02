package name.rayrobdod.stringContextParserCombinator
package internal

import scala.reflect.macros.blackbox.Context

/** Succeeds if the next input element is an `arg` with the given type; captures the expression */
private[stringContextParserCombinator]
final class OfType[Ctx <: Context with Singleton, A](
	tpetag:Ctx#TypeTag[A]
) extends Parser[Ctx, Ctx#Expr, Ctx#TypeTag, Ctx#Expr[A]] {
	private val expecting = ExpectingDescription(s"OfType(${tpetag.tpe})")

	def interpolate[ExprZ <: Ctx#Expr[_], Pos](input:Input[ExprZ, Pos])(implicit ctx: Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, Ctx#Expr[A]] = {
		input.consume(
			_ => None,
			arg => Some(arg).filter(x => x.actualType <:< tpetag.tpe).map(_.asInstanceOf[Ctx#Expr[A]]),
			this.expecting
		)
	}

	override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx: Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Ctx#Expr, Ctx#TypeTag]):Result[Unit, Pos, UnapplyExpr[Ctx, Ctx#Expr, Ctx#TypeTag, Ctx#Expr[A]]] = {
		input.consume(
			_ => None,
			(_:Unit) => Some(exprs.ofType(tpetag)),
			expecting
		)
	}
}
