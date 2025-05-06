package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.annotation.nowarn
import scala.quoted.*

// scala 2 reads the `'{Some($value}` as an unclosed character literal
// and ifdef is insufficient to hide that construct from the scala 2 compiler

private[typeclass]
final class ExprsForQuotes extends Exprs[scala.quoted.Quotes, scala.quoted.Expr, TypeCreator] {
	override def constBoolean(b: Boolean)(implicit ctx: Quotes): Expr[Boolean] = {
		implicit val quotes = ctx
		Expr(b)
	}

	override def andBooleans(left: Expr[Boolean], rightFn: () => Expr[Boolean])(implicit ctx: Quotes): Expr[Boolean] = {
		val right = rightFn()
		(left, right) match {
			case (_, Expr(true)) => left
			case (Expr(true), _) => right
			case (_, Expr(false)) => Expr(false)
			case (Expr(false), _) => Expr(false)
			case _ => '{$left && $right}
		}
	}

	override def isEqualTo[A](left: Expr[A], right: Expr[A])(implicit ctx: Quotes, typA: TypeCreator[A]): Expr[Boolean] = {
		@nowarn("msg=unused local definition") implicit val typ: Type[A] = summon[TypeCreator[A]].createType
		'{$left == $right}
	}
}
