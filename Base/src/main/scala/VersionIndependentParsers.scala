package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.{Seq, Set}
import com.rayrobdod.stringContextParserCombinator.{Parser => SCParser}

private[stringContextParserCombinator]
trait VersionIndependentParsers {
	/** The expr type of the generated parser */
	type ParserExpr
	/** The parser type, with the input parameter concretized */
	final type Parser[A] = SCParser[ParserExpr, A]

	/**
	 * Succeeds if the next character is a member of the given Set; captures that character
	 * @group PartAsChar
	 */
	def CharIn(str:Set[Char]):Parser[Char] =
		new Parser(internal.CharIn(str))

	/**
	 * Succeeds if the next character is a member of the given Seq; captures that character
	 * @group PartAsChar
	 */
	def CharIn(str:Seq[Char]):Parser[Char] =
		new Parser(internal.CharIn(str))

	/**
	 * Succeeds if the next character is a member of the given String; captures that character
	 * @group PartAsChar
	 */
	def CharIn(str:String):Parser[Char] =
		new Parser(internal.CharIn(scala.Predef.wrapString(str)))

	/**
	 * Succeeds if the next character matches the given predicate; captures that character
	 * @group PartAsChar
	 */
	def CharWhere(fn:Function1[Char, Boolean]):Parser[Char] =
		new Parser(internal.CharWhere(fn))

	/**
	 * Succeeds if the next codepoint is a member of the given Set; captures that code point
	 * @group PartAsCodepoint
	 */
	def CodePointIn(str:Set[CodePoint]):Parser[CodePoint] =
		new Parser(internal.CodePointIn(str))

	/**
	 * Succeeds if the next codepoint is a member of the given Seq; captures that code point
	 * @group PartAsCodepoint
	 */
	def CodePointIn(str:Seq[CodePoint]):Parser[CodePoint] =
		new Parser(internal.CodePointIn(str))

	/**
	 * Succeeds if the next codepoint is a member of the given string; captures that code point
	 * @group PartAsCodepoint
	 */
	def CodePointIn(str:String):Parser[CodePoint] =
		new Parser(internal.CodePointIn(str))

	/**
	 * Succeeds if the next codepoint matches the given predicate; captures that code point
	 * @group PartAsCodepoint
	 */
	def CodePointWhere(fn:Function1[CodePoint, Boolean]):Parser[CodePoint] =
		new Parser(internal.CodePointWhere(fn))

	/**
	 * Succeeds if the next set of characters in the input is equal to the given string
	 * @group Part
	 */
	def IsString(str:String):Parser[Unit] =
		new Parser(internal.IsString(str))

	/**
	 * A parser that consumes no input and always succeeds
	 * @group Constant
	 */
	def Pass:Parser[Unit] =
		new Parser(new internal.Pass)

	/**
	 * A parser that always reports a failure
	 * @group Constant
	 */
	def Fail(message:String):Parser[Nothing] =
		new Parser(new internal.Fail(ExpectingDescription(message)))

	/**
	 * A parser that succeeds iff the input is empty
	 * @group Position
	 */
	def End:Parser[Unit] =
		new Parser(new internal.End())

	/**
	 * Indirectly refers to a parser, to allow for mutual-recursion
	 * @group Misc
	 */
	def DelayedConstruction[A](fn:Function0[Parser[A]]):Parser[A] =
		new Parser(new internal.DelayedConstruction(fn))
}
