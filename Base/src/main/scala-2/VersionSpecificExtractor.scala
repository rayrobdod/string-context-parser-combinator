package name.rayrobdod.stringContextParserCombinator

import scala.annotation.nowarn
import scala.reflect.macros.blackbox.Context
import name.rayrobdod.stringContextParserCombinator.{Extractor => SCExtractor}

/**
 * Parts of [[Extractor]] that use types specific to scala 3
 */
private[stringContextParserCombinator]
trait VersionSpecificExtractor[+Expr[_], +Type[_], -A] {
	protected[stringContextParserCombinator]
	def impl: internal.Extractor[Expr, Type, A]

	/**
	 * Build an extractor that will extract values from a value of type A based on the provided StringContext
	 * @group Parse
	 */
	final def extractor[UnexprA](
		c:Context)(
		extensionClassName:String)(
		value:c.Expr[UnexprA])(
		implicit ev:c.Expr[UnexprA] <:< A,
		@nowarn("msg=never used") ev2:Expr[_] <:< c.Expr[_],
		@nowarn("msg=never used") ev3:Type[_] <:< c.TypeTag[_],
		ttUnexprA:c.TypeTag[UnexprA]
	):c.Expr[Any] = {
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
		def andBooleans(left:c.Expr[Boolean], right:c.Expr[Boolean]):c.Expr[Boolean] = {
			val B = c.universe.Unliftable.unliftBoolean
			(left.tree, right.tree) match {
				case (B(true), _) => right
				case (B(false), _) => exprFalse
				case (_, B(true)) => left
				case (_, B(false)) => exprFalse
				case (_, _) => selectApply[Boolean](left, "$amp$amp", right)
			}
		}

		def unapplyExprFlatten[Z](expr:UnapplyExpr[c.Expr, c.TypeTag, Z]):(Z => c.Expr[Boolean], List[UnapplyExpr.Part[c.Expr, c.TypeTag, Z, _]]) = {
			expr match {
				case UnapplyExpr.Empty => (_ => exprTrue, Nil)
				case UnapplyExpr.IsEqualTo(other, typ@_) => (
					value => selectApply[Boolean](value, "$eq$eq", other),
					Nil
				)
				case UnapplyExpr.OfType(typ@_) => (
					_ => exprTrue,
					UnapplyExpr.Part(typ, (value:Z) => value) :: Nil
				)
				case UnapplyExpr.Contramap(backing, mapping) => {
					val (condition, parts) = unapplyExprFlatten(backing)
					((
						mapping.andThen(condition),
						parts.map(_.contramapValue(mapping))
					))
				}
				case UnapplyExpr.WidenWith(backing, mapping) => {
					val (condition, parts) = unapplyExprFlatten(backing)
					((
						{(value:Z) =>
							val newCondition = mapping.isDefinedAt(value)
							val backingCondition = condition(mapping(value))
							andBooleans(newCondition, backingCondition)
						},
						parts.map(_.contramapValue(mapping.apply _))
					))
				}
				case UnapplyExpr.OptionallyNone(ev) => ((
					(z:Z) => ev.contraNone(z),
					Nil,
				))
				case UnapplyExpr.Sequenced(leftBacking, rightBacking, ev) => {
					val (leftCondition, leftParts) = unapplyExprFlatten(leftBacking)
					val (rightCondition, rightParts) = unapplyExprFlatten(rightBacking)
					((
						{(value:Z) =>
							val (leftValue, rightValue) = ev.separate(value)

							andBooleans(leftCondition(leftValue), rightCondition(rightValue))
						},
						leftParts.map({part => part.contramapValue((ev.separate _).andThen(_._1))}) :::
								rightParts.map({part => part.contramapValue((ev.separate _).andThen(_._2))})
					))
				}
				case UnapplyExpr.Repeated(childBackings, ev) => {
					((
						{(value:Z) =>
							val (foldingCondition, foldingValueFn) = {
								childBackings.foldLeft[(c.Expr[Boolean], () => ev.Dec)](
									(exprTrue, () => ev.contraInit(value))
								)({(folding, childBacking) =>
									val (foldingCondition, foldingValueFn) = folding

									val stopCondition = {() => ev.headTail.isDefinedAt(foldingValueFn())}

									val childAndNewValue = {() => ev.headTail.apply(foldingValueFn())}
									val childValue = {() => childAndNewValue()._1}
									val newValue = {() => childAndNewValue()._2}

									val childCondition = unapplyExprFlatten(childBacking)._1(childValue())
									val newCondition = andBooleans(foldingCondition, andBooleans(stopCondition(), childCondition))

									(newCondition, newValue)
								})
							}
							andBooleans(foldingCondition, ev.isEmpty(foldingValueFn()))
						},
						{
							childBackings.foldLeft[(List[UnapplyExpr.Part[c.Expr, c.TypeTag, ev.Dec, _]], ev.Dec => ev.Dec)](
								(Nil, {z => z})
							)({(folding, childBacking) =>
								val (previousParts, previousList) = folding
								val nextList:ev.Dec => ev.Dec = {(z:ev.Dec) => ev.headTail(previousList(z))._2}
								val childValue:ev.Dec => Any = {(z:ev.Dec) => ev.headTail(previousList(z))._1}

								val childParts = unapplyExprFlatten(childBacking)._2
										.map({part => part.contramapValue(childValue)})

								(previousParts ::: childParts, nextList)
							})._1.map(_.contramapValue({(z:Z) => ev.contraInit(z)}))
						}
					))
				}
			}
		}

		implicit val given_Position:Position[c.universe.Position] = PositionGivens.given_ExprPosition_Position(c)
		implicit val given_Ordering:Ordering[c.universe.Position] = PositionGivens.given_ExprPosition_Ordering(c)

		val ExtensionClassSelectChain = selectChain(c, extensionClassName)
		val StringContextApply = stringContextApply(c)

		import c.universe.ApplyTag
		import c.universe.SelectTag
		val strings = c.prefix.tree.duplicate match {
			case c.universe.Apply(
				ExtensionClassSelectChain(),
				List(StringContextApply(strings))
			) => {
				strings.map({x => (c.eval(x), x.tree.pos)})
			}
			case c.universe.Select(
				c.universe.Apply(
					ExtensionClassSelectChain(),
					List(StringContextApply(strings))
				),
				Name(_)
			) => {
				strings.map({x => (c.eval(x), x.tree.pos)})
			}
			case _ => c.abort(c.enclosingPosition, s"Do not know how to process this tree: " + c.universe.showRaw(c.prefix))
		}
		val args = strings.init.map(x => (((), x._2 + x._1.size)))

		val input = new Input[Unit, c.universe.Position](strings, args)

		impl.asInstanceOf[internal.Extractor[c.Expr, c.TypeTag, A]].extractor(input) match {
			case s:Success[_, _, _] => {
				val expr:UnapplyExpr[c.Expr, c.TypeTag, A] = s.choicesHead.value
				val (condition2, parts2) = unapplyExprFlatten(expr)
				val condition = ev.andThen(condition2)
				val parts = parts2.map(_.contramapValue(ev))

				parts.size match {
					case 0 =>
						AssembleUnapply.zero(c)(value, ttUnexprA, condition)
					case 1 =>
						AssembleUnapply.one(c)(value, ttUnexprA, condition, parts(0))
					case _ =>
						AssembleUnapply.many(c)(value, ttUnexprA, condition, parts)
				}
			}
			case f:Failure[c.universe.Position] => {
				reportFailure(c)(f)
			}
		}
	}
}

private[stringContextParserCombinator]
trait VersionSpecificExtractorModule {

	/**
	 * Create a Extractors that can parse Exprs belonging to the specified Context
	 * @group ExtractorGroup
	 */
	def contextExtractors(c:Context):Extractor.Extractors[c.Expr, c.TypeTag] = {
		new Extractor.Extractors[c.Expr, c.TypeTag]
				with ExprIndependentExtractors[c.Expr, c.TypeTag] {
			override def `lazy`[A](fn:Function0[SCExtractor[c.Expr, c.TypeTag, A]]):SCExtractor[c.Expr, c.TypeTag, A] =
				new SCExtractor(internal.DelayedConstruction.extractor(() => fn().impl))

			override def ofType[A](implicit tpe: c.TypeTag[A]): SCExtractor[c.Expr, c.TypeTag, c.Expr[A]] =
				new SCExtractor(new internal.OfType[c.type, A](tpe))
		}
	}
}
