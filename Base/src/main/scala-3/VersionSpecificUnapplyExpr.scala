package com.rayrobdod.stringContextParserCombinator

import scala.quoted.*

private[stringContextParserCombinator]
trait VersionSpecificUnapplyExprs {
	def forQuoted(using quotes:Quotes): UnapplyExprs[Expr, Type] = {
		val exprTrue = Expr(true)

		val andBooleanExprs = {(left:Expr[Boolean], rightFn: () => Expr[Boolean]) =>
			val right = rightFn()
			(left, right) match {
				case (_, Expr(true)) => left
				case (Expr(true), _) => right
				case (_, Expr(false)) => Expr(false)
				case (Expr(false), _) => Expr(false)
				case _ => '{$left && $right}
			}
		}

		new UnapplyExprs.Common[Expr, Type](exprTrue, Expr(false), andBooleanExprs) {
			override def ofType[Z](using tpeZ:Type[Z]):UnapplyExpr[Expr, Type, Expr[Z]] = {
				UnapplyExpr[Expr, Type, Expr[Z]](
					{(value:Expr[Z]) => exprTrue},
					UnapplyExpr.Part(tpeZ, {(value:Expr[Z]) => value}) :: Nil
				)
			}

			override def isEqualTo[A](other:Expr[A])(using Type[A]):UnapplyExpr[Expr, Type, Expr[A]] = {
				UnapplyExpr[Expr, Type, Expr[A]](
					{(value:Expr[A]) => '{$value == $other}},
					Nil
				)
			}
		}
	}
}
