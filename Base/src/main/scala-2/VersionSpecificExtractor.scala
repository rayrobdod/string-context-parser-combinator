package com.rayrobdod.stringContextParserCombinator

import scala.reflect.macros.blackbox.Context
import com.rayrobdod.stringContextParserCombinator.{Extractor => SCExtractor}

/**
 * Parts of [[Extractor]] that use types specific to scala 3
 */
private[stringContextParserCombinator]
trait VersionSpecificExtractor[Expr[_], Type[_], -A] {
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
		ev2:c.Expr[_] =:= Expr[_],
		ev3:c.TypeTag[_] =:= Type[_],
		ttUnexprA:c.TypeTag[UnexprA]
	):c.Expr[Any] = {
		val ExtensionClassSelectChain = selectChain(c, extensionClassName)
		val StringContextApply = stringContextApply(c)

		import c.universe.ApplyTag
		import c.universe.SelectTag
		val strings = c.prefix.tree.duplicate match {
			case c.universe.Apply(
				ExtensionClassSelectChain(),
				List(StringContextApply(strings))
			) => {
				strings.map({x => (c.eval(x), Position(x.tree.pos))})
			}
			case c.universe.Select(
				c.universe.Apply(
					ExtensionClassSelectChain(),
					List(StringContextApply(strings))
				),
				Name(_)
			) => {
				strings.map({x => (c.eval(x), Position(x.tree.pos))})
			}
			case _ => c.abort(c.enclosingPosition, s"Do not know how to process this tree: " + c.universe.showRaw(c.prefix))
		}
		val args = strings.init.map(x => (((), x._2 + x._1.size)))

		val input = new Input[Unit, Position.Impl](strings, args)
		implicit val exprs:UnapplyExprs[c.Expr, c.TypeTag] = UnapplyExprs.forContext(c)

		impl.asInstanceOf[internal.Extractor[c.Expr, c.TypeTag, A]].extractor(input)(implicitly, exprs) match {
			case s:Success[_, _, _] => {
				val expr:UnapplyExpr[c.Expr, c.TypeTag, A] = s.choicesHead.value
				val condition = ev.andThen(expr.condition)
				val parts = expr.parts.map(_.contramapValue(ev))

				parts.size match {
					case 0 =>
						AssembleUnapply.zero(c)(value, ttUnexprA, condition)
					case 1 =>
						AssembleUnapply.one(c)(value, ttUnexprA, condition, parts(0))
					case _ =>
						AssembleUnapply.many(c)(value, ttUnexprA, condition, parts)
				}
			}
			case f:Failure[Position.Impl] => {
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
