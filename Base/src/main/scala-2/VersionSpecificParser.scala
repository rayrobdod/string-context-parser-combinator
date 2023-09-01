package name.rayrobdod.stringContextParserCombinator

import scala.reflect.macros.blackbox.Context
import name.rayrobdod.stringContextParserCombinator.{Parser => SCPCParser}

/**
 * Parts of [[Parser]] that use types specific to scala 3
 */
private[stringContextParserCombinator]
trait VersionSpecificParser[Expr[_], Type[_], A] {
	protected[stringContextParserCombinator]
	def impl: internal.Parser[Expr, Type, A]

	/**
	 * Parses a StringContext and its arguments into a value
	 *
	 * @example
	 * {{{
	 * def valueImpl(c:Context)(args:c.Expr[Any]*):c.Expr[Result] = {
	 *   val myParser:Interpolator[Expr[Result]] = ???
	 *   myParser.interpolate(c)("package.ValueStringContext")(args)
	 * }
	 *
	 * implicit final class ValueStringContext(val sc:scala.StringContext) extends AnyVal {
	 *   def value(args:Any*):Result = macro valueImpl
	 * }
	 *
	 * // alternatively
	 * implicit final class ValueStringContext(val sc:scala.StringContext) {
	 *   object value {
	 *     def apply(args:Any*):Result = macro valueImpl
	 *   }
	 * }
	 * }}}
	 * @group Parse
	 */
	final def interpolate(c:Context)(extensionClassName:String)(args:Seq[c.Expr[Any]])(implicit ev:c.Expr[Any] <:< Expr[Any]):A = {
		new Interpolator(this.impl).interpolate(c)(extensionClassName)(args)
	}

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
		new Extractor(this.impl).extractor(c)(extensionClassName)(value)
	}
}

private[stringContextParserCombinator]
trait VersionSpecificParserModule {

	/**
	 * Create a Parsers that can parse Exprs belonging to the specified Context
	 * @group ParserGroup
	 */
	def contextParsers(c:Context):Parser.Parsers[c.Expr, c.universe.Liftable, c.TypeTag] = {
		new Parser.Parsers[c.Expr, c.universe.Liftable, c.TypeTag]
				with ExprIndependentParsers[c.Expr, c.TypeTag] {
			override def `lazy`[A](fn:Function0[SCPCParser[c.Expr, c.TypeTag, A]]):SCPCParser[c.Expr, c.TypeTag, A] =
				new SCPCParser(internal.DelayedConstruction.parser(() => fn().impl))

			override def paired[A](interpolator:Interpolator[A], extractor:Extractor[A]):SCPCParser[c.Expr, c.TypeTag, A] =
				new SCPCParser(new internal.Paired(interpolator.impl, extractor.impl))

			override def ofType[A](implicit tpe: c.TypeTag[A]): SCPCParser[c.Expr, c.TypeTag, c.Expr[A]] =
				new SCPCParser(new internal.OfType[c.type, A](tpe))
		}
	}
}
