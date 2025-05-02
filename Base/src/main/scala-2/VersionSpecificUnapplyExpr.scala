package name.rayrobdod.stringContextParserCombinator

import scala.reflect.macros.blackbox.Context

private[stringContextParserCombinator]
trait VersionSpecificUnapplyExprs {
	def forContext(c:Context): UnapplyExprs[c.type, c.Expr, c.TypeTag] = {
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

		new UnapplyExprs.Common[c.type, c.Expr, c.TypeTag]()(using typeclass.Exprs.forContext[c.type]) {
			override def ofType[Z](implicit typ:c.TypeTag[Z]):UnapplyExpr[c.type, c.Expr, c.TypeTag, c.Expr[Z]] = {
				UnapplyExpr[c.type, c.Expr, c.TypeTag, c.Expr[Z]](
					{(_:c.Expr[Z], ctx: c.type) => typeclass.Exprs.forContext[c.type].constTrue(ctx)},
					UnapplyExpr.Part(typ, {(value:c.Expr[Z], _: c.type) => value}) :: Nil
				)
			}

			override def isEqualTo[A](other:c.Expr[A])(implicit typ:c.TypeTag[A]):UnapplyExpr[c.type, c.Expr, c.TypeTag, c.Expr[A]] = {
				UnapplyExpr[c.type, c.Expr, c.TypeTag, c.Expr[A]](
					{(value:c.Expr[A], _: c.type) => selectApply[Boolean](value, "$eq$eq", other)},
					Nil
				)
			}
		}
	}
}
