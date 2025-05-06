package name.rayrobdod.stringContextParserCombinator

import com.eed3si9n.ifdef.ifdef
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
final class UnapplyExprs[Ctx, Expr[+_], Type[_]](implicit exprs: typeclass.Exprs[Ctx, Expr, Type]) {
	def empty:UnapplyExpr[Ctx, Expr, Type, Any] = {
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

	def isEqualTo[A](other:Expr[A])(implicit typ:Type[A]):UnapplyExpr[Ctx, Expr, Type, Expr[A]] = {
		UnapplyExpr[Ctx, Expr, Type, Expr[A]](
			{(value:Expr[A], ctx: Ctx) => exprs.isEqualTo(value, other)(using ctx, typ)},
			Nil
		)
	}

	def ofType[Z](implicit typ:Type[Z]):UnapplyExpr[Ctx, Expr, Type, Expr[Z]] = {
		UnapplyExpr[Ctx, Expr, Type, Expr[Z]](
			{(_:Expr[Z], ctx: Ctx) => exprs.constTrue(ctx)},
			UnapplyExpr.Part(typ, {(value:Expr[Z], _: Ctx) => value}) :: Nil
		)
	}

	def contramap[A, Z](
		backing: UnapplyExpr[Ctx, Expr, Type, A],
		mapping: (Z, Ctx) => A
	):UnapplyExpr[Ctx, Expr, Type, Z] = {
		UnapplyExpr[Ctx, Expr, Type, Z](
			(z: Z, ctx:Ctx) => backing.condition(mapping(z, ctx), ctx),
			backing.parts.map({part => part.contramapValue(mapping)})
		)
	}

	def widenWith[A, Z](
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

	def eitheredLeft[A, Z](
		leftUnapplyExpr:UnapplyExpr[Ctx, Expr, Type, A],
		ev:typeclass.ContraEithered[Ctx, Expr, A, _, Z]
	):UnapplyExpr[Ctx, Expr, Type, Z] = {
		this.widenWith(
			leftUnapplyExpr,
			ev.contraLeft
		)
	}

	def eitheredRight[B, Z](
		rightUnapplyExpr:UnapplyExpr[Ctx, Expr, Type, B],
		ev:typeclass.ContraEithered[Ctx, Expr, _, B, Z]
	):UnapplyExpr[Ctx, Expr, Type, Z] = {
		this.widenWith(
			rightUnapplyExpr,
			ev.contraRight
		)
	}

	def sequenced[A, B, Z](
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

	def repeated[A, Z](
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

	def optionallySome[A, Z](
		childUnapplyExpr:UnapplyExpr[Ctx, Expr, Type, A],
		ev:typeclass.ContraOptionally[Ctx, Expr, A, Z]
	):UnapplyExpr[Ctx, Expr, Type, Z] = {
		this.widenWith(childUnapplyExpr, ev.contraSome)
	}

	def optionallyNone[A, Z](
		ev:typeclass.ContraOptionally[Ctx, Expr, A, Z]
	):UnapplyExpr[Ctx, Expr, Type, Z] = {
		UnapplyExpr[Ctx, Expr, Type, Z]({(value, ctx) => ev.contraNone(value)(ctx)}, Nil)
	}
}

private[stringContextParserCombinator]
object UnapplyExprs {
	def forId: UnapplyExprs[IdCtx, Id, ClassTag] = new UnapplyExprs[IdCtx, Id, ClassTag]

	@ifdef("scalaEpochVersion:2")
	def forContext(c: scala.reflect.macros.blackbox.Context): UnapplyExprs[c.type, c.Expr, c.TypeTag] = {
		new UnapplyExprs[c.type, c.Expr, c.TypeTag]()(using typeclass.Exprs.forContext[c.type])
	}

	@ifdef("scalaBinaryVersion:3")
	def forQuoted: UnapplyExprs[quoted.Quotes, quoted.Expr, TypeCreator] = {
		new UnapplyExprs[quoted.Quotes, quoted.Expr, TypeCreator]()
	}
}
