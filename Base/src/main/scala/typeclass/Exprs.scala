package name.rayrobdod.stringContextParserCombinator
package typeclass

import com.eed3si9n.ifdef.ifdef
import scala.reflect.ClassTag

private[stringContextParserCombinator]
trait Exprs[Ctx, Expr[+_], Type[_]] {
	def constBoolean(b: Boolean)(implicit ctx:Ctx): Expr[Boolean]
	final def constTrue(implicit ctx:Ctx): Expr[Boolean] = this.constBoolean(true)
	final def constFalse(implicit ctx:Ctx): Expr[Boolean] = this.constBoolean(false)

	def andBooleans(left: Expr[Boolean], rightFn: () => Expr[Boolean])(implicit ctx:Ctx): Expr[Boolean]

	def isEqualTo[A](left: Expr[A], right: Expr[A])(implicit ctx: Ctx, typA: Type[A]): Expr[Boolean]
}

private[stringContextParserCombinator]
object Exprs {
	implicit val forId: Exprs[IdCtx, Id, ClassTag] = {
		new Exprs[IdCtx, Id, ClassTag] {
			override def constBoolean(b: Boolean)(implicit ctx: IdCtx): Id[Boolean] = b

			override def andBooleans(left: Boolean , rightFn: () => Boolean)(implicit ctx: IdCtx): Boolean = left && rightFn()

			override def isEqualTo[A](left: Id[A], right: Id[A])(implicit ctx: IdCtx, typA: ClassTag[A]): Id[Boolean] = left == right
		}
	}

	@ifdef("scalaEpochVersion:2")
	implicit def forContext[Ctx <: scala.reflect.macros.blackbox.Context with Singleton]: Exprs[Ctx, Ctx#Expr, Ctx#TypeTag] = {
		new Exprs[Ctx, Ctx#Expr, Ctx#TypeTag] {
			def selectApply[Z](c: Ctx)(lhs:c.Expr[_], op:String, rhs:c.Expr[_])(implicit typZ:c.TypeTag[Z]):c.Expr[Z] = {
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

			override def constBoolean(b: Boolean)(implicit ctx: Ctx): Ctx#Expr[Boolean] = {
				ctx.Expr[Boolean](ctx.universe.Liftable.liftBoolean(b))
			}

			override def andBooleans(left2: Ctx#Expr[Boolean], rightFn: () => Ctx#Expr[Boolean])(implicit ctx: Ctx): Ctx#Expr[Boolean] = {
				val B = ctx.universe.Unliftable.unliftBoolean
				val left = left2.asInstanceOf[ctx.Expr[Boolean]]
				val right = rightFn().asInstanceOf[ctx.Expr[Boolean]]
				(left.tree, right.tree) match {
					case (B(true), _) => right
					case (B(false), _) => this.constFalse
					case (_, B(true)) => left
					case (_, B(false)) => this.constFalse
					case (_, _) => selectApply[Boolean](ctx)(left, "$amp$amp", right)
				}
			}

			override def isEqualTo[A](left: Ctx#Expr[A], right: Ctx#Expr[A])(implicit ctx: Ctx, typA: Ctx#TypeTag[A]): Ctx#Expr[Boolean] = {
				val left2 = left.asInstanceOf[ctx.Expr[A]]
				val right2 = right.asInstanceOf[ctx.Expr[A]]
				selectApply[Boolean](ctx)(left2, "$eq$eq", right2)
			}
		}
	}

	@ifdef("scalaBinaryVersion:3")
	implicit val forQuoted: Exprs[scala.quoted.Quotes, scala.quoted.Expr, TypeCreator] = {
		new ExprsForQuotes
	}
}
