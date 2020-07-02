package com.rayrobdod.stringContextParserCombinator

/**
 * @group Parser
 */
trait Parser[Expr, +A] {
	def parse(input:Input[Expr]):Result[Expr, A]

	/** Returns a parser which invokes this parser, then modifies a successful result according to fn */
	def map[Z](fn:Function1[A, Z]):Parser[Expr, Z] = parsers.Map(this, fn)
	/** Returns a parser which invokes this parser, then modifies a successful result according to the parser returned by fn */
	def flatMap[Z](fn:Function1[A, Parser[Expr, Z]]):Parser[Expr, Z] = parsers.FlatMap(this, fn)
	/** Returns a parser which invokes this parser, then fails a successful result if it does not pass the predicate */
	def filter(predicate:Function1[A, Boolean], description:Expecting):Parser[Expr, A] = parsers.Filter(this, predicate, description)

	/** Returns a parser which invokes this parser, but has the given description upon failure */
	def opaque(description:Expecting):Parser[Expr, A] = parsers.Opaque(this, description)

	/** Returns a parser which invokes this parser, and upon success invokes the other parser */
	def andThen[B, Z](rhs:Parser[Expr, B])(implicit ev:Implicits.AndThenTypes[A,B,Z]):Parser[Expr, Z] = parsers.AndThen(this, rhs, ev)
	/** Returns a parser which invokes this parser and the other parser, and returns the first successful result */
	def orElse[Z >: A](rhs:Parser[Expr, Z]):Parser[Expr, Z] = parsers.OrElse(this, rhs)
	/** Returns a parser which invokes this parser repeatedly and returns the aggregated result */
	def repeat[Z](min:Int = 0, max:Int = Integer.MAX_VALUE)(implicit ev:Implicits.RepeatTypes[A, Z]):Parser[Expr, Z] = parsers.Repeat(this, min, max, ev)
	/** Returns a parser which invokes this parser and provides a value whether this parser succeeded or failed */
	def optionally[Z](implicit ev:Implicits.OptionallyTypes[A, Z]):Parser[Expr, Z] = parsers.Optionally(this, ev)
}
