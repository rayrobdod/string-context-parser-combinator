package name.rayrobdod.stringContextParserCombinator

import scala.quoted.*
import name.rayrobdod.stringContextParserCombinator.{Interpolator => SCInterpolator}

/**
 * Parts of [[Interpolator]] that use types specific to scala 3
 */
private[stringContextParserCombinator]
trait VersionSpecificInterpolator[-Expr, +A] {
	protected[stringContextParserCombinator]
	def impl: internal.Interpolator[Expr, A]

	/**
	 * Parses a StringContext and its arguments into a value
	 *
	 * @example
	 * ```
	 * extension (inline sc:StringContext)
	 *   inline def prefix(inline args:Any*):Result =
	 *     ${prefixImpl('sc, 'args)}
	 *
	 * def prefixImpl(sc:Expr[StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[Result] =
	 *   val interpolator:Interpolator[Expr[Result]] = ???
	 *   interpolator.interpolate(sc, args)
	 * ```
	 * @group Parse
	 */
	final def interpolate(sc:quoted.Expr[scala.StringContext], args:quoted.Expr[Seq[Any]])(using q:quoted.Quotes, ev:quoted.Expr[_] <:< Expr):A = {
		import scala.quoted.{Expr => _, _}
		import q.reflect.asTerm
		import PositionGivens.given

		val strings = sc match {
			case '{ _root_.scala.StringContext(${Varargs(args)}: _*) } => args
			case _ => scala.quoted.quotes.reflect.report.errorAndAbort(s"Do not know how to process this tree", sc)
		}
		val strings2 = strings.map(x => ((x.valueOrAbort, x.asTerm.pos))).toList
		val args2 = Varargs.unapply(args).get.toList.map(arg => (ev(arg), arg.asTerm.pos))

		val input = new Input[Expr, q.reflect.Position](strings2, args2)

		impl.interpolate(input) match {
			case s:Success[_, _, _] => {
				s.choicesHead.value
			}
			case f:Failure[q.reflect.Position] => {
				reportFailure(f)
			}
		}
	}
}

private[stringContextParserCombinator]
trait VersionSpecificInterpolatorModule extends ExprIndependentInterpolators[Any] {
	type Interpolator[A] = SCInterpolator[quoted.Expr[Any], A]

	/**
	 * A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree
	 * @group Arg
	 */
	def ofType[A](using Type[A], Quotes): SCInterpolator[Expr[Any], Expr[A]] =
		new SCInterpolator(new internal.OfType[A])

	/**
	 * A parser that succeeds if the next part of the in put is an `arg` and Lifter parameterized on `arg`'s type can be implicitly summoned
	 *
	 * The implicitly summoned value and the `arg` value are passed to `lift`; the returned value is returned by this parser
	 * @group Arg
	 */
	def lifted[Lifter[_], Z](lift:LiftFunction[Expr, Type, Lifter, Z], description:String)(using Quotes, Type[Lifter]):SCInterpolator[Expr[Any], Z] =
		new SCInterpolator(internal.Lifted(lift, ExpectingDescription(description)))


	/**
	 * Create an Interpolators that can parse `quoted.Expr`s
	 * @group InterpolatorGroup
	 */
	def quotedInterpolators(using Quotes):Interpolator.Interpolators[quoted.Expr, quoted.ToExpr, quoted.Type] & LiftedInterpolator[quoted.Expr, quoted.Type] = {
		new Interpolator.Interpolators[quoted.Expr, quoted.ToExpr, quoted.Type]
				with ExprIndependentInterpolators[quoted.Expr[Any]]
				with LiftedInterpolator[quoted.Expr, quoted.Type]
		{
			override def `lazy`[A](fn:Function0[SCInterpolator[quoted.Expr[Any], A]]):SCInterpolator[quoted.Expr[Any], A] =
				new SCInterpolator(internal.DelayedConstruction.interpolator(fn))

			override def ofType[A](implicit tpe: Type[A]): SCInterpolator[Expr[Any], Expr[A]] =
				new SCInterpolator(new internal.OfType[A])

			override def lifted[Lifter[_], Z](lift:LiftFunction[quoted.Expr, quoted.Type, Lifter, Z], description:String)(using quoted.Type[Lifter]):SCInterpolator[Expr[Any], Z] =
				new SCInterpolator(internal.Lifted(lift, ExpectingDescription(description)))
		}
	}

	/**
	 *
	 * @group InterpolatorGroup
	 */
	trait LiftedInterpolator[Expr[+_], Type[_ <: AnyKind]] {
		/**
		 * A parser that succeeds if the next part of the in put is an `arg` and Lifter parameterized on `arg`'s type can be implicitly summoned
		 *
		 * The implicitly summoned value and the `arg` value are passed to `lift`; the returned value is returned by this parser
		 * @group Arg
		 */
		def lifted[Lifter[_], Z](lift:LiftFunction[Expr, Type, Lifter, Z], description:String)(using Type[Lifter]):SCInterpolator[Expr[Any], Z]
	}
}
