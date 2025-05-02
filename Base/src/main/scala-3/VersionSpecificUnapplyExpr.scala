package name.rayrobdod.stringContextParserCombinator

import scala.annotation.nowarn
import scala.quoted.*

private[stringContextParserCombinator]
trait VersionSpecificUnapplyExprs {
	def forQuoted: UnapplyExprs[Quotes, Expr, TypeCreator] = {
		new UnapplyExprs.Common[Quotes, Expr, TypeCreator]() {
			override def ofType[Z](using tpeZ:TypeCreator[Z]):UnapplyExpr[Quotes, Expr, TypeCreator, Expr[Z]] = {
				UnapplyExpr[Quotes, Expr, TypeCreator, Expr[Z]](
					{(value:Expr[Z], ctx: Quotes) =>
						implicit val ctx2: Quotes = ctx
						typeclass.Exprs.forQuoted.constTrue
					},
					UnapplyExpr.Part(tpeZ, {(value:Expr[Z], _:Quotes) => value}) :: Nil
				)
			}

			override def isEqualTo[A](other:Expr[A])(using TypeCreator[A]):UnapplyExpr[Quotes, Expr, TypeCreator, Expr[A]] = {
				UnapplyExpr[Quotes, Expr, TypeCreator, Expr[A]](
					{(value:Expr[A], ctx: Quotes) =>
						implicit val ctx2: Quotes = ctx
						@nowarn("msg=unused local definition") implicit val typ: Type[A] = summon[TypeCreator[A]].createType
						'{$value == $other}
					},
					Nil
				)
			}
		}
	}
}
