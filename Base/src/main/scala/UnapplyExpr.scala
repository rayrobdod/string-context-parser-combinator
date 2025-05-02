package name.rayrobdod.stringContextParserCombinator

import scala.reflect.ClassTag

/**
 * The data needed to create an Unapply
 */
private[stringContextParserCombinator]
final case class UnapplyExpr[Ctx, +Expr[+_], +Type[_], -A] (
	condition: (A, Ctx) => Expr[Boolean],
	parts: List[UnapplyExpr.Part[Ctx, Expr, Type, A, _]]
)

private[stringContextParserCombinator]
object UnapplyExpr {
	final case class Part[Ctx, +Expr[+_], +Type[_], -A, Z](typ: Type[Z], value: (A, Ctx) => Expr[Z]) {
		def contramapValue[B](contrafn: (B, Ctx) => A):Part[Ctx, Expr, Type, B, Z] = new Part(typ, (b: B, ctx:Ctx) => value(contrafn(b, ctx), ctx))
	}
}

private[stringContextParserCombinator]
trait UnapplyExprs[Ctx, Expr[+_], Type[_]] {
	def empty:UnapplyExpr[Ctx, Expr, Type, Any]

	def isChar(expecting:Char):UnapplyExpr[Ctx, Expr, Type, Char]

	def isCodePoint(expecting:CodePoint):UnapplyExpr[Ctx, Expr, Type, CodePoint]

	def isEqualTo[A](other:Expr[A])(implicit typ:Type[A]):UnapplyExpr[Ctx, Expr, Type, Expr[A]]

	def ofType[Z](implicit typ:Type[Z]):UnapplyExpr[Ctx, Expr, Type, Expr[Z]]

	def contramap[A, Z](
		backing: UnapplyExpr[Ctx, Expr, Type, A],
		mapping:(Z, Ctx) => A
	):UnapplyExpr[Ctx, Expr, Type, Z]

	def widenWith[A, Z](
		backing: UnapplyExpr[Ctx, Expr, Type, A],
		mapping: PartialExprFunction[Ctx, Expr, Z, A]
	):UnapplyExpr[Ctx, Expr, Type, Z]

	def eitheredLeft[A, Z](
		leftUnapplyExpr:UnapplyExpr[Ctx, Expr, Type, A],
		ev:typeclass.ContraEithered[Ctx, Expr, A, _, Z]
	):UnapplyExpr[Ctx, Expr, Type, Z]

	def eitheredRight[B, Z](
		rightUnapplyExpr:UnapplyExpr[Ctx, Expr, Type, B],
		ev:typeclass.ContraEithered[Ctx, Expr, _, B, Z]
	):UnapplyExpr[Ctx, Expr, Type, Z]

	def sequenced[A, B, Z](
		leftUnapplyExpr:UnapplyExpr[Ctx, Expr, Type, A],
		rightUnapplyExpr:UnapplyExpr[Ctx, Expr, Type, B],
		ev:typeclass.ContraSequenced[Ctx, A, B, Z]
	):UnapplyExpr[Ctx, Expr, Type, Z]

	def repeated[A, Z](
		childUnapplyExprs:List[UnapplyExpr[Ctx, Expr, Type, A]],
		separator:typeclass.ContraRepeated[Ctx, Expr, A, Z]
	):UnapplyExpr[Ctx, Expr, Type, Z]

	def optionallySome[A, Z](
		childUnapplyExpr:UnapplyExpr[Ctx, Expr, Type, A],
		ev:typeclass.ContraOptionally[Ctx, Expr, A, Z]
	):UnapplyExpr[Ctx, Expr, Type, Z]

	def optionallyNone[A, Z](
		ev:typeclass.ContraOptionally[Ctx, Expr, A, Z]
	):UnapplyExpr[Ctx, Expr, Type, Z]
}

private[stringContextParserCombinator]
object UnapplyExprs extends VersionSpecificUnapplyExprs {
	private[stringContextParserCombinator]
	abstract class Common[Ctx, Expr[+_], Type[_]](
		implicit exprs: typeclass.Exprs[Ctx, Expr]
	) extends UnapplyExprs[Ctx, Expr, Type] {
		override def empty:UnapplyExpr[Ctx, Expr, Type, Any] = {
			UnapplyExpr[Ctx, Expr, Type, Any]((_:Any, ctx:Ctx) => exprs.constTrue(ctx), Nil)
		}

		def isChar(expecting:Char):UnapplyExpr[Ctx, Expr, Type, Char] = {
			UnapplyExpr[Ctx, Expr, Type, Char](
				{(scrutinee:Char, ctx:Ctx) => exprs.constBoolean(expecting == scrutinee)(ctx)},
				Nil
			)
		}

		def isCodePoint(expecting:CodePoint):UnapplyExpr[Ctx, Expr, Type, CodePoint] = {
			UnapplyExpr[Ctx, Expr, Type, CodePoint](
				{(scrutinee:CodePoint, ctx:Ctx) => exprs.constBoolean(expecting == scrutinee)(ctx)},
				Nil
			)
		}

		override def contramap[A, Z](
			backing: UnapplyExpr[Ctx, Expr, Type, A],
			mapping: (Z, Ctx) => A
		):UnapplyExpr[Ctx, Expr, Type, Z] = {
			UnapplyExpr[Ctx, Expr, Type, Z](
				(z: Z, ctx:Ctx) => backing.condition(mapping(z, ctx), ctx),
				backing.parts.map({part => part.contramapValue(mapping)})
			)
		}

		override def widenWith[A, Z](
			backing: UnapplyExpr[Ctx, Expr, Type, A],
			mapping: PartialExprFunction[Ctx, Expr, Z, A]
		):UnapplyExpr[Ctx, Expr, Type, Z] = {
			UnapplyExpr[Ctx, Expr, Type, Z](
				{(value:Z, ctx:Ctx) =>
					implicit val ctx2 = ctx
					val newCondition = mapping.isDefinedAt(value)
					val backingCondition = {() => backing.condition(mapping(value), ctx)}
					exprs.andBooleans(newCondition, backingCondition)
				},
				backing.parts.map({part => part.contramapValue[Z]((a, ctx:Ctx) => mapping.apply(a)(ctx))})
			)
		}

		override def eitheredLeft[A, Z](
			leftUnapplyExpr:UnapplyExpr[Ctx, Expr, Type, A],
			ev:typeclass.ContraEithered[Ctx, Expr, A, _, Z]
		):UnapplyExpr[Ctx, Expr, Type, Z] = {
			this.widenWith(
				leftUnapplyExpr,
				ev.contraLeft
			)
		}

		override def eitheredRight[B, Z](
			rightUnapplyExpr:UnapplyExpr[Ctx, Expr, Type, B],
			ev:typeclass.ContraEithered[Ctx, Expr, _, B, Z]
		):UnapplyExpr[Ctx, Expr, Type, Z] = {
			this.widenWith(
				rightUnapplyExpr,
				ev.contraRight
			)
		}

		override def sequenced[A, B, Z](
			leftUnapplyExpr:UnapplyExpr[Ctx, Expr, Type, A],
			rightUnapplyExpr:UnapplyExpr[Ctx, Expr, Type, B],
			ev:typeclass.ContraSequenced[Ctx, A, B, Z]
		):UnapplyExpr[Ctx, Expr, Type, Z] = {
			UnapplyExpr[Ctx, Expr, Type, Z](
				{(value:Z, ctx:Ctx) =>
					val (leftValue, rightValue) = ev.separate(value)(using ctx)

					val leftCondition = leftUnapplyExpr.condition(leftValue, ctx)
					val rightCondition = rightUnapplyExpr.condition(rightValue, ctx)

					exprs.andBooleans(leftCondition, () => rightCondition)(ctx)
				},
				leftUnapplyExpr.parts.map({part => part.contramapValue[Z]({(value, ctx) => ev.separate(value)(using ctx)._1})}) :::
						rightUnapplyExpr.parts.map({part => part.contramapValue[Z]({(value, ctx) => ev.separate(value)(using ctx)._2})})
			)
		}

		override def repeated[A, Z](
			childUnapplyExprs:List[UnapplyExpr[Ctx, Expr, Type, A]],
			separator:typeclass.ContraRepeated[Ctx, Expr, A, Z]
		):UnapplyExpr[Ctx, Expr, Type, Z] = {
			UnapplyExpr[Ctx, Expr, Type, Z](
				{(value:Z, ctx:Ctx) =>
					implicit val ctx2: Ctx = ctx
					val (foldingCondition, foldingValueFn) = {
						childUnapplyExprs.foldLeft[(Expr[Boolean], () => Z)](
							(exprs.constTrue, () => value)
						)({(folding, childUnapplyExpr) =>
							val (foldingCondition, foldingValueFn) = folding

							val stopCondition = {() => separator.headTail.isDefinedAt(foldingValueFn())}

							val childAndNewValue = {() => separator.headTail.apply(foldingValueFn())}
							val childValue = {() => childAndNewValue()._1}
							val newValue = {() => childAndNewValue()._2}

							val childCondition = {() => childUnapplyExpr.condition(childValue(), ctx)}
							val newCondition = exprs.andBooleans(foldingCondition, {() => exprs.andBooleans(stopCondition(), childCondition)})

							(newCondition, newValue)
						})
					}
					exprs.andBooleans(foldingCondition, () => separator.isEmpty(foldingValueFn()))
				},
				{
					childUnapplyExprs.foldLeft[(List[UnapplyExpr.Part[Ctx, Expr, Type, Z, _]], (Z, Ctx) => Z)](
						(Nil, {(z, _:Ctx) => z})
					)({(folding, childUnapplyExpr) =>
						val (previousParts, previousList) = folding
						val nextList:(Z, Ctx) => Z = {(z:Z, ctx:Ctx) => separator.headTail(previousList(z, ctx))(ctx)._2}
						val childValue:(Z, Ctx) => A = {(z:Z, ctx:Ctx) => separator.headTail(previousList(z, ctx))(ctx)._1}

						val childParts = childUnapplyExpr.parts.map({part => part.contramapValue(childValue)})

						(previousParts ::: childParts, nextList)
					})._1
				}
			)
		}

		override def optionallySome[A, Z](
			childUnapplyExpr:UnapplyExpr[Ctx, Expr, Type, A],
			ev:typeclass.ContraOptionally[Ctx, Expr, A, Z]
		):UnapplyExpr[Ctx, Expr, Type, Z] = {
			this.widenWith(childUnapplyExpr, ev.contraSome)
		}

		override def optionallyNone[A, Z](
			ev:typeclass.ContraOptionally[Ctx, Expr, A, Z]
		):UnapplyExpr[Ctx, Expr, Type, Z] = {
			UnapplyExpr[Ctx, Expr, Type, Z]({(value, ctx) => ev.contraNone(value)(ctx)}, Nil)
		}
	}

	def forId: UnapplyExprs[IdCtx, Id, ClassTag] = {
		new UnapplyExprs.Common[IdCtx, Id, ClassTag] {
			override def ofType[Z](implicit typ:ClassTag[Z]):UnapplyExpr[IdCtx, Id, ClassTag, Id[Z]] = {
				UnapplyExpr[IdCtx, Id, ClassTag, Id[Z]](
					{(_:Id[Z], _: IdCtx) => true},
					UnapplyExpr.Part(typ, {(value:Id[Z], _: IdCtx) => value}) :: Nil
				)
			}

			override def isEqualTo[A](other:Id[A])(implicit typ:ClassTag[A]):UnapplyExpr[IdCtx, Id, ClassTag, Id[A]] = {
				UnapplyExpr[IdCtx, Id, ClassTag, Id[A]](
					{(value:Id[A], _: IdCtx) => value == other},
					Nil
				)
			}
		}
	}
}
