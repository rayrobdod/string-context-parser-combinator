package com.rayrobdod.stringContextParserCombinator

/**
 * Parses an interpolated string into a value
 *
 * @group Parser
 * @tparam Expr the macro-level expression type.
 * @tparam A the type of the parsed result
 */
trait Parser[Expr, +A] {
	private[stringContextParserCombinator]
	def parse(input:Input[Expr]):Result[Expr, A]

	/** Returns a parser which invokes this parser, then modifies a successful result according to fn */
	def map[Z](fn:Function1[A, Z]):Parser[Expr, Z] =
		new parsers.Map(this, fn)

	/** Returns a parser which invokes this parser, then modifies a successful result according to the parser returned by fn */
	def flatMap[Z](fn:Function1[A, Parser[Expr, Z]]):Parser[Expr, Z] =
		new parsers.FlatMap(this, fn)

	/** Returns a parser which invokes this parser, then fails a successful result if it does not pass the predicate */
	def filter(predicate:Function1[A, Boolean], description:String):Parser[Expr, A] =
		new parsers.Filter(this, predicate, ExpectingDescription(description))


	/** Returns a parser which invokes this parser, but has the given description upon failure */
	def opaque(description:String):Parser[Expr, A] =
		new parsers.Opaque(this, ExpectingDescription(description))


	/** Returns a parser which invokes this parser, and upon success invokes the other parser.
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param rhs the parser to call after this one
	 * @param ev A descriptor of how to combine two values into one value
	 */
	def andThen[B, Z](rhs:Parser[Expr, B])(implicit ev:typelevel.Sequenced[A,B,Z]):Parser[Expr, Z] =
		new parsers.AndThen(this, rhs, ev)

	/** Returns a parser which invokes this parser, and upon success invokes the other parser.
	 * If the second parser fails, the error is marked so that prior
	 * `orElse` parsers will not try other branches.
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param rhs the parser to call after this one
	 * @param ev A descriptor of how to combine two values into one value
	 */
	def andThenWithCut[B, Z](rhs:Parser[Expr, B])(implicit ev:typelevel.Sequenced[A,B,Z]):Parser[Expr, Z] =
		new parsers.AndThenWithCut(this, rhs, ev)

	/** Returns a parser which invokes this parser, and if this parser fails tries the other parser, and returns the first successful result
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param rhs the parser to call after this one
	 * @param ev A descriptor of how to treat either value as one value
	 */
	def orElse[B, Z](rhs:Parser[Expr, B])(implicit ev:typelevel.Eithered[A,B,Z]):Parser[Expr, Z] =
		new parsers.OrElse(this, rhs, ev)

	/** Returns a parser which invokes this parser repeatedly and returns the aggregated result
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param min the minimum number of repeats to be considered successful
	 * @param max the maximum number of repeats to consume
	 * @param delimiter a parser describing separators between each repeat. Defaults to a parser that consumes no input.
	 * @param ev A descriptor of how to combine the repeated values into one value
	 */
	def repeat[Z](min:Int = 0, max:Int = Integer.MAX_VALUE, delimiter:Parser[Expr, Unit] = parsers.NilParser)(implicit ev:typelevel.Repeated[A, Z]):Parser[Expr, Z] =
		new parsers.Repeat(this, min, max, delimiter, ev)

	/** Returns a parser which invokes this parser and provides a value whether this parser succeeded or failed
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param ev A descriptor of how to mark present or absent values
	 */
	def optionally[Z](implicit ev:typelevel.Optionally[A, Z]):Parser[Expr, Z] =
		parsers.Optionally(this, ev)
}
