package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Set
import scala.collection.immutable.Seq

/**
 * Parses an interpolated string expression into some value
 *
 * @tparam Expr the macro-level expression type.
 * @tparam A the type of the parsed result
 *
 * @groupname Parse parse
 * @groupprio Parse 100
 * @groupname Map Result Changing Combinators
 * @groupprio Map 1010
 * @groupname Sequence Sequencing Combinators
 * @groupprio Sequence 1020
 * @groupname Branch Branching Combinators
 * @groupprio Branch 1030
 * @groupname Filter Filtering Combinators
 * @groupprio Filter 1040
 * @groupname Repeat Repeating Combinators
 * @groupprio Repeat 1050
 * @groupname ErrorPlus Error Enriching Combinators
 * @groupprio ErrorPlus 1060
 * @groupname Misc Other Combinators
 * @groupprio Misc 1999
 */
final class Interpolator[-Expr, +A] private[stringContextParserCombinator] (
		protected[stringContextParserCombinator] override val impl: internal.Interpolator[Expr, A]
) extends VersionSpecificInterpolator[Expr, A] {

	/**
	 * Returns a parser which invokes this parser, then modifies a successful result according to fn
	 * @group Map
	 */
	def map[Z](fn:Function1[A, Z]):Interpolator[Expr, Z] =
		new Interpolator(new internal.Map(this.impl, fn))

	/**
	 * Returns a parser which invokes this parser, then maps a successful result by lifting the
	 * successful result into an Expr
	 * @group Map
	 */
	def mapToExpr[ExprZ[_], Z >: A, ToExpr[_], Type[_]](
		implicit mapping:typeclass.ToExprMapping[ExprZ, ToExpr, Type],
		toExpr:ToExpr[Z],
		tpe:Type[Z]
	):Interpolator[Expr, ExprZ[Z]] = {
		this.map(value => mapping(value, toExpr, tpe))
	}

	/**
	 * Returns a parser which invokes this parser, then modifies a successful result according to the parser returned by fn
	 * @group Sequence
	 */
	def flatMap[ExprZ <: Expr, Z](fn:Function1[A, Interpolator[ExprZ, Z]]):Interpolator[ExprZ, Z] =
		new Interpolator(new internal.FlatMap(this.impl, fn))

	/**
	 * Returns a parser which invokes this parser, then fails a successful result if it does not pass the predicate
	 * @group Filter
	 */
	def filter(predicate:Function1[A, Boolean], description:String):Interpolator[Expr, A] =
		new Interpolator(new internal.Filter(this.impl, predicate, ExpectingDescription(description)))


	/**
	 * Returns a parser which invokes this parser, but has the given description upon failure
	 * @group ErrorPlus
	 */
	def opaque(description:String):Interpolator[Expr, A] =
		new Interpolator(new internal.Opaque(this.impl, ExpectingDescription(description)))

	/**
	 * Returns a parser which invokes this parser,
	 * but treats the result of a failed parse as if it does not consume input
	 * @group Misc
	 */
	def attempt:Interpolator[Expr, A] =
		new Interpolator(new internal.Attempt(this.impl))

	/**
	 * Returns a parser which invokes this parser, and upon success invokes the other parser.
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param rhs the parser to call after this one
	 * @param ev A descriptor of how to combine two values into one value
	 * @group Sequence
	 */
	def andThen[ExprZ <: Expr, B, Z](rhs:Interpolator[ExprZ, B])(implicit ev:typeclass.Sequenced[A,B,Z]):Interpolator[ExprZ, Z] =
		new Interpolator(new internal.AndThen(this.impl, rhs.impl, ev))

	/**
	 * Returns a parser which invokes this parser, and then:
	 *   * If this parser run succeeded, return this internal's success
	 *   * If this parser failed and consumed input, return this parser's failure
	 *   * If this parser failed but did not consume input, run the other parser and return the other parser's result
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param rhs the parser to call after this one
	 * @param ev A descriptor of how to treat either value as one value
	 * @group Branch
	 */
	def orElse[ExprZ <: Expr, B, Z](rhs:Interpolator[ExprZ, B])(implicit ev:typeclass.Eithered[A,B,Z]):Interpolator[ExprZ, Z] =
		new Interpolator(new internal.OrElse(this.impl, rhs.impl, ev))

	/**
	 * Returns a parser which invokes this parser repeatedly and returns the aggregated result
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param min the minimum number of repeats to be considered successful
	 * @param max the maximum number of repeats to consume
	 * @param delimiter a parser describing separators between each repeat. Defaults to a parser that always succeeds and consumes no input.
	 * @param strategy whether the repeat will attempt to match as much or as little as possible, and whether it will backtrack. Default is [[RepeatStrategy.Possessive]]
	 * @param ev A descriptor of how to combine the repeated values into one value
	 * @group Repeat
	 */
	def repeat[ExprZ <: Expr, Z](
		min:Int = 0,
		max:Int = Integer.MAX_VALUE,
		delimiter:Interpolator[ExprZ, Unit] = new Interpolator[ExprZ, Unit](new internal.Pass),
		strategy:RepeatStrategy = RepeatStrategy.Possessive)(
		implicit ev:typeclass.Repeated[A, Z]
	):Interpolator[ExprZ, Z] =
		new Interpolator(new internal.Repeat(this.impl, min, max, delimiter.impl, strategy, ev))

	/**
	 * Returns a parser which invokes this parser and provides a value whether this parser succeeded or failed
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param strategy whether the optionally will attempt to match as much or as little as possible, and whether it will backtrack. Default is [[RepeatStrategy.Possessive]]
	 * @param ev A descriptor of how to mark present or absent values
	 * @group Repeat
	 */
	def optionally[Z](
		strategy:RepeatStrategy = RepeatStrategy.Possessive)(
		implicit ev:typeclass.Optionally[A, Z]
	):Interpolator[Expr, Z] =
		new Interpolator(internal.Optionally(this.impl, strategy, ev))
}

object Interpolator extends VersionSpecificInterpolatorModule {
	/**
	 * A trait that provides Interpolator factory methods that conform to a particular input Expr type parameter.
	 *
	 * In scala 3, the Interpolator companion object directly extends this trait,
	 * and as such this would generally by calling methods directly on Interpolator.
	 * However, since in scala 2 the Expr depends on a particular instance of `blackbox.Context`,
	 * the Interpolator companion object instead has a `scoped` method that takes a Context and returns a ScopedInterpolators for that particular Context.
	 *
	 * @groupname Part String-Part
	 * @groupprio Part 100
	 * @groupname PartAsChar String-Part as Char
	 * @groupprio PartAsChar 110
	 * @groupname PartAsCodepoint String-Part as Codepoint
	 * @groupprio PartAsCodepoint 120
	 * @groupname Arg Argument-Part
	 * @groupprio Arg 200
	 * @groupname Constant Constant
	 * @groupprio Constant 300
	 * @groupname Position Position
	 * @groupprio Position 400
	 * @groupname Misc Miscellaneous
	 * @groupprio Misc 999
	 */
	trait ScopedInterpolators extends VersionSpecificScopedInterpolators {
		// Requires that VersionSpecificScopedInterpolators defines a
		// `type Interpolator[A] = Interpolator[???, A]`

		/**
		 * Succeeds if the next character is a member of the given Set; captures that character
		 * @group PartAsChar
		 */
		def CharIn(str:Set[Char]):Interpolator[Char] =
			new Interpolator(internal.CharIn(str))

		/**
		 * Succeeds if the next character is a member of the given Seq; captures that character
		 * @group PartAsChar
		 */
		def CharIn(str:Seq[Char]):Interpolator[Char] =
			new Interpolator(internal.CharIn(str))

		/**
		 * Succeeds if the next character is a member of the given String; captures that character
		 * @group PartAsChar
		 */
		def CharIn(str:String):Interpolator[Char] =
			new Interpolator(internal.CharIn(scala.Predef.wrapString(str)))

		/**
		 * Succeeds if the next character matches the given predicate; captures that character
		 * @group PartAsChar
		 */
		def CharWhere(fn:Function1[Char, Boolean]):Interpolator[Char] =
			new Interpolator(internal.CharWhere(fn))

		/**
		 * Succeeds if the next codepoint is a member of the given Set; captures that code point
		 * @group PartAsCodepoint
		 */
		def CodePointIn(str:Set[CodePoint]):Interpolator[CodePoint] =
			new Interpolator(internal.CodePointIn(str))

		/**
		 * Succeeds if the next codepoint is a member of the given Seq; captures that code point
		 * @group PartAsCodepoint
		 */
		def CodePointIn(str:Seq[CodePoint]):Interpolator[CodePoint] =
			new Interpolator(internal.CodePointIn(str))

		/**
		 * Succeeds if the next codepoint is a member of the given string; captures that code point
		 * @group PartAsCodepoint
		 */
		def CodePointIn(str:String):Interpolator[CodePoint] =
			new Interpolator(internal.CodePointIn(str))

		/**
		 * Succeeds if the next codepoint matches the given predicate; captures that code point
		 * @group PartAsCodepoint
		 */
		def CodePointWhere(fn:Function1[CodePoint, Boolean]):Interpolator[CodePoint] =
			new Interpolator(internal.CodePointWhere(fn))

		/**
		 * Succeeds if the next set of characters in the input is equal to the given string
		 * @group Part
		 */
		def IsString(str:String):Interpolator[Unit] =
			new Interpolator(internal.IsString(str))

		/**
		 * A parser that consumes no input and always succeeds
		 * @group Constant
		 */
		def Pass:Interpolator[Unit] =
			new Interpolator(new internal.Pass)

		/**
		 * A parser that always reports a failure
		 * @group Constant
		 */
		def Fail(message:String):Interpolator[Nothing] =
			new Interpolator(new internal.Fail(ExpectingDescription(message)))

		/**
		 * A parser that succeeds iff the input is empty
		 * @group Position
		 */
		def End:Interpolator[Unit] =
			new Interpolator(new internal.End())

		/**
		 * Indirectly refers to a parser, to allow for mutual-recursion
		 * @group Misc
		 */
		def DelayedConstruction[A](fn:Function0[Interpolator[A]]):Interpolator[A] =
			new Interpolator(new internal.DelayedConstruction(fn))
	}
}
