package com.rayrobdod.stringContextParserCombinator

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
final class Parser[-Expr, +A] private[stringContextParserCombinator] (
		protected[stringContextParserCombinator] override val impl: internal.Parser[Expr, A]
) extends VersionSpecificParser[Expr, A] {

	/**
	 * Returns a parser which invokes this parser, then modifies a successful result according to fn
	 * @group Map
	 */
	def map[Z](fn:Function1[A, Z]):Parser[Expr, Z] =
		new Parser(new internal.Map(this.impl, fn))

	/**
	 * Returns a parser which invokes this parser, then modifies a successful result according to the parser returned by fn
	 * @group Sequence
	 */
	def flatMap[ExprZ <: Expr, Z](fn:Function1[A, Parser[ExprZ, Z]]):Parser[ExprZ, Z] =
		new Parser(new internal.FlatMap(this.impl, fn))

	/**
	 * Returns a parser which invokes this parser, then fails a successful result if it does not pass the predicate
	 * @group Filter
	 */
	def filter(predicate:Function1[A, Boolean], description:String):Parser[Expr, A] =
		new Parser(new internal.Filter(this.impl, predicate, ExpectingDescription(description)))


	/**
	 * Returns a parser which invokes this parser, but has the given description upon failure
	 * @group ErrorPlus
	 */
	def opaque(description:String):Parser[Expr, A] =
		new Parser(new internal.Opaque(this.impl, ExpectingDescription(description)))

	/**
	 * Returns a parser which invokes this parser,
	 * but treats the result of a failed parse as if it does not consume input
	 * @group Misc
	 */
	def attempt:Parser[Expr, A] =
		new Parser(new internal.Attempt(this.impl))

	/**
	 * Returns a parser which invokes this parser, and upon success invokes the other parser.
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param rhs the parser to call after this one
	 * @param ev A descriptor of how to combine two values into one value
	 * @group Sequence
	 */
	def andThen[ExprZ <: Expr, B, Z](rhs:Parser[ExprZ, B])(implicit ev:typeclass.Sequenced[A,B,Z]):Parser[ExprZ, Z] =
		new Parser(new internal.AndThen(this.impl, rhs.impl, ev))

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
	def orElse[ExprZ <: Expr, B, Z](rhs:Parser[ExprZ, B])(implicit ev:typeclass.Eithered[A,B,Z]):Parser[ExprZ, Z] =
		new Parser(new internal.OrElse(this.impl, rhs.impl, ev))

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
		delimiter:Parser[ExprZ, Unit] = new Parser[ExprZ, Unit](new internal.Pass),
		strategy:RepeatStrategy = RepeatStrategy.Possessive)(
		implicit ev:typeclass.Repeated[A, Z]
	):Parser[ExprZ, Z] =
		new Parser(new internal.Repeat(this.impl, min, max, delimiter.impl, strategy, ev))

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
	):Parser[Expr, Z] =
		new Parser(internal.Optionally(this.impl, strategy, ev))
}
