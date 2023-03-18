package com.rayrobdod.stringContextParserCombinator

import scala.reflect.macros.blackbox.Context

/**
 * Parts of [[Parser]] that use types specific to scala 2
 */
private[stringContextParserCombinator]
trait VersionSpecificParser[-Expr, +A] {
	protected[stringContextParserCombinator]
	def impl: internal.Parser[Expr, A]

	/**
	 * Parses a StringContext and its arguments into a value
	 *
	 * @example
	 * {{{
	 * def valueImpl(c:Context)(args:c.Expr[Any]*):c.Expr[Result] = {
	 *   val myParser:Parser[Expr[Result]] = ???
	 *   myParser.interpolate(c)("package.ValueStringContext")(args)
	 * }
	 *
	 * implicit final class ValueStringContext(val sc:scala.StringContext) extends AnyVal {
	 *   def value(args:Any*):Result = macro valueImpl
	 * }
	 * }}}
	 * @group Parse
	 */
	final def interpolate(c:Context)(extensionClassName:String)(args:Seq[c.Expr[Any]])(implicit ev:c.Expr[_] <:< Expr):A = {
		val ExtensionClassSelectChain = selectChain(c, extensionClassName)
		val StringContextApply = stringContextApply(c)

		import c.universe.ApplyTag
		val strings = c.prefix.tree.duplicate match {
			case c.universe.Apply(
				ExtensionClassSelectChain(),
				List(StringContextApply(strings))
			) => {
				strings.map({x => (c.eval(x), Position(x.tree.pos))})
			}
			case _ => c.abort(c.enclosingPosition, s"Do not know how to process this tree: " + c.universe.showRaw(c.prefix))
		}

		val input = new Input[Expr, Position.Impl](strings, args.toList.map(arg => (ev(arg), Position(arg.tree.pos))))

		impl.interpolate(input) match {
			case s:Success[_, _, _] => {
				s.choicesHead.value
			}
			case f:Failure[Position.Impl] => {
				reportFailure(c)(f)
			}
		}
	}
}
