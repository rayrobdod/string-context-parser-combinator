package com.rayrobdod.stringContextParserCombinator

import scala.reflect.macros.blackbox.Context
import com.rayrobdod.stringContextParserCombinator.{Interpolator => SCInterpolator}

/**
 * Parts of [[Interpolator]] that use types specific to scala 2
 */
private[stringContextParserCombinator]
trait VersionSpecificInterpolator[-Expr, +A] {
	protected[stringContextParserCombinator]
	def impl: internal.Interpolator[Expr, A]

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
	final def interpolate(c:Context)(extensionClassName:String)(args:Seq[c.Expr[Any]])(implicit ev:c.Expr[_] <:< Expr):A = {
		val ExtensionClassSelectChain = selectChain(c, extensionClassName)
		val StringContextApply = stringContextApply(c)

		import c.universe.ApplyTag
		val strings = c.prefix.tree.duplicate match {
			// for methods on the extension object
			case c.universe.Apply(
				ExtensionClassSelectChain(),
				List(StringContextApply(strings))
			) => {
				strings.map({x => (c.eval(x), Position(x.tree.pos))})
			}
			// for `apply` methods on an object in the extension object
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
trait VersionSpecificInterpolatorModule {
	/**
	 * Create a ScopedInterpolators for the provided Context
	 */
	def scoped(c:Context):Interpolator.ScopedInterpolators {
		type Context = c.type
	} = new Interpolator.ScopedInterpolators {
		type Context = c.type
		override val ctx:Context = c
	}
}

private[stringContextParserCombinator]
trait VersionSpecificScopedInterpolators {
	type Context <: scala.reflect.macros.blackbox.Context
	val ctx:Context
	/** The expr type of the created parsers */
	type InterpolatorExpr = ctx.Expr[_]
	/** The parser type, with the input parameter concretized */
	type Interpolator[A] = SCInterpolator[InterpolatorExpr, A]

	/**
	 * A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree
	 * @group Arg
	 */
	def OfType[A](implicit tpe:ctx.TypeTag[A]):Interpolator[ctx.Expr[A]] =
		new SCInterpolator(new internal.OfType[ctx.type, A](tpe))

	/**
	 * A parser that succeeds if the next part of the in put is an `arg` and Lifter parameterized on `arg`'s type can be implicitly summoned
	 *
	 * The implicitly summoned value and the `arg` value are passed to `lift`; the returned value is returned by this parser
	 * @group Arg
	 */
	def Lifted[Lifter[_], Z](lift:LiftFunction[ctx.type, Lifter, Z], description:String)(implicit lifterTypeTag:ctx.TypeTag[Lifter[_]]):Interpolator[Z] =
		new SCInterpolator(internal.Lifted(ctx)(lift, ExpectingDescription(description)))
}
