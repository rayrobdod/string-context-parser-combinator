package com.rayrobdod.stringContextParserCombinator

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/**
 * @group Parser
 */
trait Parser[U <: Context with Singleton, +A] {
	def parse(input:Input[U#Expr[_]]):Result[U#Expr[_], A]

	/** Returns a parser which invokes this parser, then modifies a successful result according to fn */
	def map[Z](fn:Function1[A, Z]):Parser[U, Z] = parsers.Map(this, fn)
	/** Returns a parser which invokes this parser, then modifies a successful result according to the parser returned by fn */
	def flatMap[Z](fn:Function1[A, Parser[U, Z]]):Parser[U, Z] = parsers.FlatMap(this, fn)
	/** Returns a parser which invokes this parser, then fails a successful result if it does not pass the predicate */
	def filter(predicate:Function1[A, Boolean], description:String):Parser[U, A] = parsers.Filter(this, predicate, Failure.Leaf(description))

	/** Returns a parser which invokes this parser, but has the given description upon failure */
	def opaque(description:String):Parser[U, A] = this.opaque(Failure.Leaf(description))
	private[stringContextParserCombinator] def opaque(description:Failure.Expecting) = parsers.Opaque(this, description)

	/** Returns a parser which invokes this parser, and upon success invokes the other parser */
	def andThen[B, Z](rhs:Parser[U, B])(implicit ev:Implicits.AndThenTypes[A,B,Z]):Parser[U, Z] = parsers.AndThen(this, rhs, ev)
	/** Returns a parser which invokes this parser and the other parser, and returns the first successful result */
	def orElse[Z >: A](rhs:Parser[U, Z]):Parser[U, Z] = parsers.OrElse(this, rhs)
	/** Returns a parser which invokes this parser repeatedly and returns the aggregated result */
	def repeat[Z](min:Int = 0, max:Int = Integer.MAX_VALUE)(implicit ev:Implicits.RepeatTypes[A, Z]):Parser[U, Z] = parsers.Repeat(this, min, max, ev)
	/** Returns a parser which invokes this parser and provides a value whether this parser succeeded or failed */
	def optionally[Z](implicit ev:Implicits.OptionallyTypes[A, Z]):Parser[U, Z] = parsers.Optionally(this, ev)
}
