package name.rayrobdod.stringContextParserCombinator
package internal

import scala.reflect.ClassTag

/** Succeeds if the next input element is an `arg` with the given type; captures the value */
private[stringContextParserCombinator]
final class OfClass[A](
	clazz:ClassTag[A]
) extends Parser[IdCtx, Id, ClassTag, A] {
	private val expecting = ExpectingDescription(s"OfType(${clazz.runtimeClass.getName})")

	override def interpolate[ExprZ <: Any, Pos](input:Input[ExprZ, Pos])(implicit ctx:IdCtx, ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
		input.consume(
			_ => None,
			arg => clazz.unapply(arg),
			this.expecting
		)
	}

	override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:IdCtx, ev1:Ordering[Pos], exprs:UnapplyExprs[IdCtx, Id, ClassTag]):Result[Unit, Pos, UnapplyExpr[IdCtx, Id, ClassTag, Id[A]]] = {
		input.consume(
			_ => None,
			(_:Unit) => Some(exprs.ofType(clazz)),
			expecting
		)
	}
}
