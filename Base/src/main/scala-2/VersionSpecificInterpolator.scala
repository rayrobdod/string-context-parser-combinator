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
		import c.universe.SelectTag
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
	 * Create a Interpolators that can parse Exprs belonging to the specified Context
	 * @group InterpolatorGroup
	 */
	def contextInterpolators(c:Context):Interpolator.Interpolators[c.Expr, c.universe.Liftable, c.TypeTag] with LiftedInterpolator[c.type] = {
		new Interpolator.Interpolators[c.Expr, c.universe.Liftable, c.TypeTag]
				with ExprIndependentInterpolators[c.Expr[Any]]
				with LiftedInterpolator[c.type] {
			override def `lazy`[A](fn:Function0[Interpolator[A]]):Interpolator[A] =
				new Interpolator[A](internal.DelayedConstruction.interpolator(fn))

			override def ofType[A](implicit tpe: c.TypeTag[A]): Interpolator[c.Expr[A]] =
				new Interpolator[c.Expr[A]](new internal.OfType[c.type, A](tpe))

			override def lifted[Lifter[_], Z](lift:LiftFunction[c.type, Lifter, Z], description:String)(implicit lifterTypeTag:c.TypeTag[Lifter[_]]):Interpolator[Z] =
				new Interpolator[Z](internal.Lifted(c)(lift, ExpectingDescription(description)))
		}
	}

	/**
	 *
	 * @group InterpolatorGroup
	 */
	trait LiftedInterpolator[C <: Context with Singleton] {
		/**
		 * A parser that succeeds if the next part of the in put is an `arg` and Lifter parameterized on `arg`'s type can be implicitly summoned
		 *
		 * The implicitly summoned value and the `arg` value are passed to `lift`; the returned value is returned by this parser
		 * @group Arg
		 */
		def lifted[Lifter[_], Z](lift:LiftFunction[C, Lifter, Z], description:String)(implicit lifterTypeTag:C#TypeTag[Lifter[_]]):SCInterpolator[C#Expr[Any], Z]
	}
}
