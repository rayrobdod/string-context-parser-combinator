package com.rayrobdod.stringContextParserCombinator

import scala.quoted.*
import com.rayrobdod.stringContextParserCombinator.{Extractor => SCExtractor, Unapply => SCUnapply}

/**
 * Parts of [[Extractor]] that use types specific to scala 3
 */
private[stringContextParserCombinator]
trait VersionSpecificExtractor[Expr[_], Type[_], -A] {
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
		quoted.Expr[Boolean] =:= Expr[Boolean],
		quoted.Type[Boolean] =:= Type[Boolean],
	):quoted.Expr[Unapply[UnexprA]] = {
		val strings = sc match {
			case '{ _root_.scala.StringContext(${Varargs(args)}: _*) } => args
			case _ => scala.quoted.quotes.reflect.report.errorAndAbort(s"Do not know how to process this tree", sc)
		}
		val strings2 = strings.map(x => ((x.valueOrAbort, Position(x)))).toList
		val args2 = strings2.init.map(x => (((), x._2 + x._1.size)))

		val input = new Input[Unit, Position.Impl](strings2, args2)
		implicit val exprs:UnapplyExprs[quoted.Expr, quoted.Type] = UnapplyExprs.forQuoted

		impl.asInstanceOf[internal.Extractor[quoted.Expr, quoted.Type, A]].extractor(input) match {
			case s:Success[_, _, _] => {
				val unexpr = summon[quoted.Expr[UnexprA] <:< A]

				val expr:UnapplyExpr[quoted.Expr, quoted.Type, quoted.Expr[UnexprA]] = unexpr.substituteContra(s.choicesHead.value)
				val conditionFn:quoted.Expr[UnexprA] => quoted.Expr[Boolean] = expr.condition

				expr.parts.size match {
					case 0 =>
						'{((a:UnexprA) => ${conditionFn('a)}):Unapply.Zero[UnexprA]}
					case 1 =>
						'{((a:UnexprA) => Option.when[Any](${conditionFn('a)})(${expr.parts(0).value('a)})):Unapply.Fixed[UnexprA, Any]}
					case _ =>
						import quotes.reflect._
						val unexpraTypeTree = TypeTree.of[UnexprA]

						val tupleTypeConstructorSymbol = defn.TupleClass(expr.parts.size)
						val tupleTypeConstructorTree = TypeIdent(tupleTypeConstructorSymbol)
						val tupleTypeTree = Applied(tupleTypeConstructorTree, expr.parts.map(part => TypeTree.of(using part.typ)))
						val optionTupleTypeTree = Applied(TypeIdent(defn.OptionClass), List(tupleTypeTree))

						val tupleModule = tupleTypeConstructorSymbol.companionModule
						val tupleConstructorTree = Ref(tupleModule)
						val partsFn:quoted.Expr[UnexprA] => List[quoted.Expr[Any]] = {a => expr.parts.map(partFn => partFn.value(a))}
						val tupleTreeFn = partsFn.andThen({parts =>
							tupleConstructorTree
								.select(tupleModule.methodMember("apply")(0))
								.appliedToTypeTrees(parts.map(_ => TypeIdent(defn.AnyClass)))
								.appliedToArgs(parts.map(_.asTerm))
						})

						val unapplyTypeConstructorTree = TypeIdent(Symbol.requiredClass("com.rayrobdod.stringContextParserCombinator.Unapply.Fixed"))
						val unapplyTypeTreeFn = tupleTreeFn.andThen(tupleTree => Applied(unapplyTypeConstructorTree, List(unexpraTypeTree, tupleTree)))

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
											val typeArgs = expr.parts.map(part => TypeTree.of(using part.typ))
											val valueArgs = expr.parts.map(part => part.value(param).asTerm)

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
			case f:Failure[Position.Impl] => {
				reportFailure(f)
			}
		}
	}
}

private[stringContextParserCombinator]
trait VersionSpecificExtractorModule extends ExprIndependentExtractors[Expr, Type] {
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
