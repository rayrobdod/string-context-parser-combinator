package name.rayrobdod.stringContextParserCombinator

import scala.quoted.*
import name.rayrobdod.stringContextParserCombinator.{Extractor => SCExtractor, Unapply => SCUnapply}

/**
 * Parts of [[Extractor]] that use types specific to scala 3
 */
private[stringContextParserCombinator]
trait VersionSpecificExtractor[+Expr[_], +Type[_], -A] {
	protected[stringContextParserCombinator]
	def impl: internal.Extractor[Expr, Type, A]

	/**
	 * Parses a StringContext into an extractor
	 *
	 * @example
	 * ```
	 * def valueImpl(sc:Expr[scala.StringContext])(using Quotes):Expr[Unapply[Result]] = {
	 *   val myParser:Extractor[Expr[Result]] = ???
	 *   myParser.extractor(sc)
	 * }
	 *
	 * extension (inline sc:scala.StringContext)
	 *	  inline def value:Unapply[Result] =
	 *	    ${valueImpl('sc)}
	 * ```
	 * @group Parse
	 */
	final def extractor[UnexprA](
		sc:quoted.Expr[scala.StringContext]
	)(using
		quoted.Quotes,
		quoted.Type[UnexprA],
		quoted.Expr[UnexprA] <:< A,
		Expr[Boolean] <:< quoted.Expr[Boolean],
		Type[Boolean] <:< quoted.Type[Boolean],
	):quoted.Expr[Unapply[UnexprA]] = {
		import scala.quoted.{Expr => _, _}
		import quotes.reflect.asTerm
		import PositionGivens.given

		def andBooleans(left:quoted.Expr[Boolean], right:quoted.Expr[Boolean]):quoted.Expr[Boolean] = {
			(left, right) match {
				case (quoted.Expr(true), _) => right
				case (quoted.Expr(false), _) => quoted.Expr(false)
				case (_, quoted.Expr(true)) => left
				case (_, quoted.Expr(false)) => quoted.Expr(false)
				case (_, _) => '{$left && $right}
			}
		}

		def unapplyExprFlatten[Z](expr:UnapplyExpr[quoted.Expr, quoted.Type, Z]):(Z => quoted.Expr[Boolean], List[UnapplyExpr.Part[quoted.Expr, quoted.Type, Z, _]]) = {
			expr match {
				case UnapplyExpr.Empty => (_ => quoted.Expr(true), Nil)
				case expr@UnapplyExpr.IsEqualTo(other, typ) => {
					@annotation.nowarn given quoted.Type[expr.Z2] = typ
					(
						(value:quoted.Expr[_]) => '{$value == $other},
						Nil
					)
				}
				case UnapplyExpr.OfType(typ@_) => (
					_ => quoted.Expr(true),
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
				case expr@UnapplyExpr.Repeated(childBackings, ev) => {
					((
						{(value:Z) =>
							val (foldingCondition, foldingValueFn) = {
								childBackings.foldLeft[(quoted.Expr[Boolean], () => ev.Dec)](
									(quoted.Expr(true), () => ev.contraInit(value))
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
							childBackings.foldLeft[(List[UnapplyExpr.Part[quoted.Expr, quoted.Type, ev.Dec, _]], ev.Dec => ev.Dec)](
								(Nil, {z => z})
							)({(folding, childBacking) =>
								val (previousParts, previousList) = folding
								val nextList:ev.Dec => ev.Dec = {(z:ev.Dec) => ev.headTail(previousList(z))._2}
								val childValue:ev.Dec => expr.A2 = {(z:ev.Dec) => ev.headTail(previousList(z))._1}

								val childParts = unapplyExprFlatten(childBacking)._2
										.map({part => part.contramapValue(childValue)})

								(previousParts ::: childParts, nextList)
							})._1.map(_.contramapValue({(z:Z) => ev.contraInit(z)}))
						}
					))
				}
			}
		}

		val strings = sc match {
			case '{ _root_.scala.StringContext(${Varargs(args)}: _*) } => args
			case _ => scala.quoted.quotes.reflect.report.errorAndAbort(s"Do not know how to process this tree", sc)
		}
		val strings2 = strings.map(x => ((x.valueOrAbort, x.asTerm.pos))).toList
		val args2 = strings2.init.map(x => (((), x._2 + x._1.size)))

		val input = new Input[Unit, quotes.reflect.Position](strings2, args2)

		impl.asInstanceOf[internal.Extractor[quoted.Expr, quoted.Type, A]].extractor(input) match {
			case s:Success[_, _, _] => {
				val unexpr = summon[quoted.Expr[UnexprA] <:< A]

				val expr:UnapplyExpr[quoted.Expr, quoted.Type, quoted.Expr[UnexprA]] = unexpr.substituteContra(s.choicesHead.value)
				val (conditionFn, parts) = unapplyExprFlatten(expr)

				parts.size match {
					case 0 =>
						'{((a:UnexprA) => ${conditionFn('a)}):Unapply.Zero[UnexprA]}
					case 1 =>
						'{((a:UnexprA) => Option.when[Any](${conditionFn('a)})(${parts(0).value('a)})):Unapply.Fixed[UnexprA, Any]}
					case _ =>
						import quotes.reflect._
						val unexpraTypeTree = TypeTree.of[UnexprA]

						val tupleTypeConstructorSymbol = defn.TupleClass(parts.size)
						val tupleTypeConstructorTree = TypeIdent(tupleTypeConstructorSymbol)
						val tupleTypeTree = Applied(tupleTypeConstructorTree, parts.map(part => TypeTree.of(using part.typ)))
						val optionTupleTypeTree = Applied(TypeIdent(defn.OptionClass), List(tupleTypeTree))

						val tupleModule = tupleTypeConstructorSymbol.companionModule
						val tupleConstructorTree = Ref(tupleModule)

						val unapplyTypeConstructorTree = TypeIdent(Symbol.requiredClass("name.rayrobdod.stringContextParserCombinator.Unapply.Fixed"))

						val anonfunType = MethodType(
							List("a"))(
							{(_:MethodType) => List(unexpraTypeTree.tpe)},
							{(_:MethodType) => optionTupleTypeTree.tpe},
						)
						val anonfunSymbol = Symbol.newMethod(
							Symbol.spliceOwner,
							"$anonfun",
							anonfunType
						)

						Block(
							List(
								DefDef(
									anonfunSymbol,
									{paramss =>
										val param = paramss(0)(0).asExprOf[UnexprA]
										val condition = conditionFn(param).asTerm
										val optionModule = defn.OptionClass.companionModule

										val partsTuple = {
											val typeArgs = parts.map(part => TypeTree.of(using part.typ))
											val valueArgs = parts.map(part => part.value(param).asTerm)

											tupleConstructorTree
												.select(tupleModule.methodMember("apply")(0))
												.appliedToTypeTrees(typeArgs)
												.appliedToArgs(valueArgs)
										}

										Option(
											Ref(optionModule)
												.select(optionModule.methodMember("when")(0))
												.appliedToType(tupleTypeTree.tpe)
												.appliedToArgss(
													List(List(condition), List(partsTuple))
												)
										)
									}
								),
							),
							Closure(
								Ref(anonfunSymbol),
								Option(AppliedType(unapplyTypeConstructorTree.tpe, List(unexpraTypeTree.tpe, tupleTypeTree.tpe)))
							)
						)
						.asExprOf[SCUnapply[UnexprA]]
				}
			}
			case f:Failure[quotes.reflect.Position] => {
				reportFailure(f)
			}
		}
	}
}

private[stringContextParserCombinator]
trait VersionSpecificExtractorModule {
	type Extractor[A] = SCExtractor[quoted.Expr, quoted.Type, A]

	/**
	 * A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree
	 * @group Arg
	 */
	def ofType[A](using Type[A], Quotes): SCExtractor[Expr, Type, Expr[A]] =
		new SCExtractor(new internal.OfType[A])

	/**
	 * Create an Extractors that can parse `quoted.Expr`s
	 * @group ExtractorGroup
	 */
	def quotedExtractors(using Quotes):Extractor.Extractors[Expr, Type] = {
		new Extractor.Extractors[Expr, Type]
				with ExprIndependentExtractors[Expr, Type] {
			override def `lazy`[A](fn:Function0[SCExtractor[Expr, Type, A]]):SCExtractor[Expr, Type, A] =
				new SCExtractor(internal.DelayedConstruction.extractor(() => fn().impl))

			override def ofType[A](implicit tpe: Type[A]): SCExtractor[Expr, Type, Expr[A]] =
				new SCExtractor(new internal.OfType[A])
		}
	}
}
