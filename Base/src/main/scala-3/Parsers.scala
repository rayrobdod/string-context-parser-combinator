package com.rayrobdod.stringContextParserCombinator

import scala.language.higherKinds
import scala.quoted.*

/**
 * Methods to create leaf parsers
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
object Parsers extends VersionIndependentParsers {
	type ParserExpr = quoted.Expr[_]

	/**
	 * A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree
	 * @group Arg
	 */
	def OfType[A](using Type[A], Quotes):Parser[quoted.Expr[A]] =
		new Parser(new internal.OfType[A])

	/**
	 * A parser that succeeds if the next part of the in put is an `arg` and Lifter parameterized on `arg`'s type can be implicitly summoned
	 *
	 * The implicitly summoned value and the `arg` value are passed to `lift`; the returned value is returned by this parser
	 * @group Arg
	 */
	def Lifted[Lifter[_], Z](lift:LiftFunction[Lifter, Z], description:String)(using Type[Lifter], Quotes):Parser[Z] =
		new Parser(internal.Lifted(lift, ExpectingDescription(description)))
}
