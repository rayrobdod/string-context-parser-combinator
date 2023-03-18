package com.rayrobdod.stringContextParserCombinator

/**
 * Parts of [[Parser]] that use types specific to scala 3
 */
private[stringContextParserCombinator]
trait VersionSpecificParser[-Expr, +A] {
	protected[stringContextParserCombinator]
	def impl: internal.Parser[Expr, A]

	/**
	 * Parses a StringContext and its arguments into a value
	 *
	 * @example
	 * ```
	 * def valueImpl(sc:Expr[scala.StringContext],
	 *         args:Expr[Seq[Any]])(using Quotes):Expr[Result] = {
	 *   val myParser:Parser[Expr[Result]] = ???
	 *   myParser.interpolate(sc, args)
	 * }
	 *
	 * extension (inline sc:scala.StringContext)
	 *	  inline def value(inline args:Any*):Result =
	 *	    ${valueImpl('sc, 'args)}
	 * ```
	 * @group Parse
	 */
	final def interpolate(sc:quoted.Expr[scala.StringContext], args:quoted.Expr[Seq[Any]])(using q:quoted.Quotes, ev:quoted.Expr[_] <:< Expr):A = {
		import scala.quoted.{Expr => _, _}
		val strings = sc match {
			case '{ _root_.scala.StringContext(${Varargs(args)}: _*) } => args
			case _ => scala.quoted.quotes.reflect.report.errorAndAbort(s"Do not know how to process this tree", sc)
		}
		val strings2 = strings.map(x => ((x.valueOrAbort, Position(x)))).toList
		val args2 = Varargs.unapply(args).get.toList.map(arg => (ev(arg), Position(arg)))

		val input = new Input[Expr, Position.Impl](strings2, args2)

		impl.interpolate(input) match {
			case s:Success[_, _, _] => {
				s.choicesHead.value
			}
			case f:Failure[Position.Impl] => {
				reportFailure(f)
			}
		}
	}
}
