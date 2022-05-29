package com.rayrobdod.stringContextParserCombinator

/**
 * Parses an interpolated string expression into some value
 *
 * @tparam Expr the macro-level expression type.
 * @tparam A the type of the parsed result
 */
trait Parser[-Expr, +A] {
	private[stringContextParserCombinator]
	def parse[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos]):Result[ExprZ, Pos, A]

	/**
	 * Returns a parser which invokes this parser, then modifies a successful result according to fn
	 */
	def map[Z](fn:Function1[A, Z]):Parser[Expr, Z] =
		new parsers.Map(this, fn)

	/**
	 * Returns a parser which invokes this parser, then modifies a successful result according to the parser returned by fn
	 */
	def flatMap[ExprZ <: Expr, Z](fn:Function1[A, Parser[ExprZ, Z]]):Parser[ExprZ, Z] =
		new parsers.FlatMap(this, fn)

	/**
	 * Returns a parser which invokes this parser, then fails a successful result if it does not pass the predicate
	 */
	def filter(predicate:Function1[A, Boolean], description:String):Parser[Expr, A] =
		new parsers.Filter(this, predicate, ExpectingDescription(description))


	/**
	 * Returns a parser which invokes this parser, but has the given description upon failure
	 */
	def opaque(description:String):Parser[Expr, A] =
		new parsers.Opaque(this, ExpectingDescription(description))


	/**
	 * Returns a parser which invokes this parser, and upon success invokes the other parser.
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param rhs the parser to call after this one
	 * @param ev A descriptor of how to combine two values into one value
	 */
	def andThen[ExprZ <: Expr, B, Z](rhs:Parser[ExprZ, B])(implicit ev:typelevel.Sequenced[A,B,Z]):Parser[ExprZ, Z] =
		new parsers.AndThen(this, rhs, ev)

	/**
	 * Returns a parser which invokes this parser, and upon success invokes the other parser.
	 * If the second parser fails, the error is marked so that prior
	 * `orElse` parsers will not try other branches.
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param rhs the parser to call after this one
	 * @param ev A descriptor of how to combine two values into one value
	 */
	def andThenWithCut[ExprZ <: Expr, B, Z](rhs:Parser[ExprZ, B])(implicit ev:typelevel.Sequenced[A,B,Z]):Parser[ExprZ, Z] =
		new parsers.AndThenWithCut(this, rhs, ev)

	/**
	 * Returns a parser which invokes this parser, and if this parser fails tries the other parser, and returns the first successful result
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param rhs the parser to call after this one
	 * @param ev A descriptor of how to treat either value as one value
	 */
	def orElse[ExprZ <: Expr, B, Z](rhs:Parser[ExprZ, B])(implicit ev:typelevel.Eithered[A,B,Z]):Parser[ExprZ, Z] =
		new parsers.OrElse(this, rhs, ev)

	/**
	 * Returns a parser which invokes this parser repeatedly and returns the aggregated result
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param min the minimum number of repeats to be considered successful
	 * @param max the maximum number of repeats to consume
	 * @param delimiter a parser describing separators between each repeat. Defaults to a parser that always succeeds and consumes no input.
	 * @param strategy whether the repeat will attempt to match as much or as little as possible, and whether it will backtrack. Default is [[RepeatStrategy.Possessive]]
	 * @param ev A descriptor of how to combine the repeated values into one value
	 */
	def repeat[ExprZ <: Expr, Z](
		min:Int = 0,
		max:Int = Integer.MAX_VALUE,
		delimiter:Parser[ExprZ, Unit] = parsers.Pass,
		strategy:RepeatStrategy = RepeatStrategy.Possessive)(
		implicit ev:typelevel.Repeated[A, Z]
	):Parser[ExprZ, Z] =
		new parsers.Repeat(this, min, max, delimiter, strategy, ev)

	/**
	 * Returns a parser which invokes this parser and provides a value whether this parser succeeded or failed
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param strategy whether the optionally will attempt to match as much or as little as possible, and whether it will backtrack. Default is [[RepeatStrategy.Possessive]]
	 * @param ev A descriptor of how to mark present or absent values
	 */
	def optionally[Z](
		strategy:RepeatStrategy = RepeatStrategy.Possessive)(
		implicit ev:typelevel.Optionally[A, Z]
	):Parser[Expr, Z] =
		parsers.Optionally(this, strategy, ev)
}
