package com.rayrobdod.stringContextParserCombinator

import scala.reflect.macros.blackbox.Context
import com.rayrobdod.stringContextParserCombinator.{Parser => SCParser}

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

private[stringContextParserCombinator]
trait VersionSpecificParserModule {
	/**
	 * Create a ScopedParsers for the provided Context
	 */
	def scoped(c:Context):Parser.ScopedParsers {
		type Context = c.type
	} = new Parser.ScopedParsers {
		type Context = c.type
		override val ctx:Context = c
	}
}

private[stringContextParserCombinator]
trait VersionSpecificScopedParsers {
	type Context <: scala.reflect.macros.blackbox.Context
	val ctx:Context
	/** The expr type of the created parsers */
	type ParserExpr = ctx.Expr[_]
	/** The parser type, with the input parameter concretized */
	type Parser[A] = SCParser[ParserExpr, A]

	/**
	 * A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree
	 * @group Arg
	 */
	def OfType[A](implicit tpe:ctx.TypeTag[A]):Parser[ctx.Expr[A]] =
		new SCParser(new internal.OfType[ctx.type, A](tpe))

	/**
	 * A parser that succeeds if the next part of the in put is an `arg` and Lifter parameterized on `arg`'s type can be implicitly summoned
	 *
	 * The implicitly summoned value and the `arg` value are passed to `lift`; the returned value is returned by this parser
	 * @group Arg
	 */
	def Lifted[Lifter[_], Z](lift:LiftFunction[ctx.type, Lifter, Z], description:String)(implicit lifterTypeTag:ctx.TypeTag[Lifter[_]]):Parser[Z] =
		new SCParser(internal.Lifted(ctx)(lift, ExpectingDescription(description)))
}
