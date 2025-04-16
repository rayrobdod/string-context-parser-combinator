package name.rayrobdod.stringContextParserCombinator

import scala.annotation.nowarn
import scala.quoted.*
import name.rayrobdod.stringContextParserCombinator.{Unapply => SCUnapply}

// scala 2 reads the `'{Some($value}` as an unclosed character literal
// and ifdef is insufficient to hide that construct from the scala 2 compiler

private[stringContextParserCombinator]
object InterpolatorImpl {
	def stringContextFromExpr(sc:Expr[StringContext])(using Quotes): Seq[Expr[String]] = {
		sc match {
			case '{ _root_.scala.StringContext(${Varargs(args)}: _*) } => args
			case _ => scala.quoted.quotes.reflect.report.errorAndAbort(s"Do not know how to process this tree", sc)
		}
	}

	def unapplyExprToExpr[UnexprA](expr: UnapplyExpr[quoted.Expr, quoted.Type, quoted.Expr[UnexprA]])(using Type[UnexprA], Quotes): Expr[SCUnapply[UnexprA]] = {
		val conditionFn:quoted.Expr[UnexprA] => quoted.Expr[Boolean] = expr.condition

		expr.parts match {
			case Nil =>
				'{((a:UnexprA) => ${conditionFn('a)}):Unapply.Zero[UnexprA]}
			case (part: UnapplyExpr.Part[quoted.Expr, quoted.Type, quoted.Expr[UnexprA], z]) :: Nil =>
				@nowarn("msg=unused local definition") given quoted.Type[z] = part.typ
				'{((a:UnexprA) => Option.when[z](${conditionFn('a)})(${part.value('a)})):Unapply.Fixed[UnexprA, z]}
			case _ =>
				import quotes.reflect._
				val unexpraTypeTree = TypeTree.of[UnexprA]

				val tupleTypeConstructorSymbol = defn.TupleClass(expr.parts.size)
				val tupleTypeConstructorTree = TypeIdent(tupleTypeConstructorSymbol)
				val tupleTypeTree = Applied(tupleTypeConstructorTree, expr.parts.map(part => TypeTree.of(using part.typ)))
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
}
