package com.rayrobdod.stringContextParserCombinator

import scala.reflect.macros.blackbox.Context

private[stringContextParserCombinator]
trait VersionSpecificUnapplyExprs {
	def forContext(c:Context): UnapplyExprs[c.Expr, c.TypeTag] = {
		def selectApply[Z](lhs:c.Expr[_], op:String, rhs:c.Expr[_])(implicit typZ:c.TypeTag[Z]):c.Expr[Z] = {
			c.Expr[Z](
				c.universe.Apply(
					c.universe.Select(
						lhs.tree,
						c.universe.TermName(op)
					),
					List(rhs.tree)
				)
			)
		}

		val exprTrue = c.Expr[Boolean](c.universe.Liftable.liftBoolean(true))
		val exprFalse = c.Expr[Boolean](c.universe.Liftable.liftBoolean(false))

		val andBooleanExprs:Function2[c.Expr[Boolean], () => c.Expr[Boolean], c.Expr[Boolean]] = {(left, rightFn) =>
			val B = c.universe.Unliftable.unliftBoolean
			val right = rightFn()
			(left.tree, right.tree) match {
				case (B(true), _) => right
				case (B(false), _) => c.Expr[Boolean](c.universe.Liftable.liftBoolean(false))
				case (_, B(true)) => left
				case (_, B(false)) => c.Expr[Boolean](c.universe.Liftable.liftBoolean(false))
				case (_, _) => selectApply(left, "$amp$amp", right)
			}
		}

		new UnapplyExprs.Common[c.Expr, c.TypeTag](exprTrue, exprFalse, andBooleanExprs) {
			override def ofType[Z](implicit typ:c.TypeTag[Z]):UnapplyExpr[c.Expr, c.TypeTag, c.Expr[Z]] = {
				UnapplyExpr[c.Expr, c.TypeTag, c.Expr[Z]](
					{(_:c.Expr[Z]) => exprTrue},
					UnapplyExpr.Part(typ, {(value:c.Expr[Z]) => value}) :: Nil
				)
			}

			override def isEqualTo[A](other:c.Expr[A])(implicit typ:c.TypeTag[A]):UnapplyExpr[c.Expr, c.TypeTag, c.Expr[A]] = {
				UnapplyExpr[c.Expr, c.TypeTag, c.Expr[A]](
					{(value:c.Expr[A]) => selectApply[Boolean](value, "$eq$eq", other)},
					Nil
				)
			}
		}
	}
}
