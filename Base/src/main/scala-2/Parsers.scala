package com.rayrobdod.stringContextParserCombinator

/**
 * A trait that provides Parser factory methods that conform to a particular input type parameter.
 *
 * The trait can either be used as a Mixin
 *
 * {{{
 * object Parsers extends stringContextParserCombinator.Parsers {
 *   def LowerAlpha:Parser[Char] = CharIn('a' to 'z')
 * }
 * Parsers.LowerAlpha
 * }}}
 *
 * or an instance can be created with the companion object's apply method,
 * and the methods can be imported from that instance
 *
 * {{{
 * val p = stringContextParserCombinator.Parsers(c)
 * import p._
 * val lowerAlpha:Parser[Char] = CharIn('a' to 'z')
 * lowerAlpha
 * }}}
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
trait Parsers extends VersionIndependentParsers {
	type Context <: scala.reflect.macros.blackbox.Context
	val ctx:Context
	/** The parser type, with the input parameter concretized */
	type ParserExpr = ctx.Expr[_]

	/**
	 * A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree
	 * @group Arg
	 */
	def OfType[A](implicit tpe:ctx.TypeTag[A]):Parser[ctx.Expr[A]] =
		new Parser(new internal.OfType[ctx.type, A](tpe))

	/**
	 * A parser that succeeds if the next part of the in put is an `arg` and Lifter parameterized on `arg`'s type can be implicitly summoned
	 *
	 * The implicitly summoned value and the `arg` value are passed to `lift`; the returned value is returned by this parser
	 * @group Arg
	 */
	def Lifted[Lifter[_], Z](lift:LiftFunction[ctx.type, Lifter, Z], description:String)(implicit lifterTypeTag:ctx.TypeTag[Lifter[_]]):Parser[Z] =
		new Parser(internal.Lifted(ctx)(lift, ExpectingDescription(description)))
}

/**
 */
object Parsers {
	def apply(c:scala.reflect.macros.blackbox.Context):Parsers {
		type Context = c.type
	} = new Parsers {
		type Context = c.type
		override val ctx:Context = c
	}
}

