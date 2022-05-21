package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.{Seq, Set}
import com.rayrobdod.{stringContextParserCombinator => scpc}

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
 */
trait Parsers {
	type Context <: scala.reflect.macros.blackbox.Context
	val ctx:Context
	/** The parser type, with the input parameter concretized */
	type Parser[A] =
			scpc.Parser[ctx.Expr[_], A]

	/** Succeeds if the next character is a member of the given Set; captures that character */
	def CharIn(str:Set[Char]):Parser[Char] =
		parsers.CharIn(str)

	/** Succeeds if the next character is a member of the given Seq; captures that character */
	def CharIn(str:Seq[Char]):Parser[Char] =
		parsers.CharIn(str)

	/** Succeeds if the next character is a member of the given String; captures that character */
	def CharIn(str:String):Parser[Char] =
		parsers.CharIn(scala.Predef.wrapString(str))

	/** Succeeds if the next character matches the given predicate; captures that character */
	def CharWhere(fn:Function1[Char, Boolean], description:String):Parser[Char] =
		parsers.CharWhere(fn, ExpectingDescription(description))

	/** Succeeds if the next codepoint is a member of the given Set; captures that code point */
	def CodePointIn(str:Set[CodePoint]):Parser[CodePoint] =
		parsers.CodePointIn(str)

	/** Succeeds if the next codepoint is a member of the given Seq; captures that code point */
	def CodePointIn(str:Seq[CodePoint]):Parser[CodePoint] =
		parsers.CodePointIn(str)

	/** Succeeds if the next codepoint is a member of the given string; captures that code point */
	def CodePointIn(str:String):Parser[CodePoint] =
		parsers.CodePointIn(str)

	/** Succeeds if the next codepoint matches the given predicate; captures that code point */
	def CodePointWhere(fn:Function1[CodePoint, Boolean], description:String):Parser[CodePoint] =
		parsers.CodePointWhere(fn, ExpectingDescription(description))

	/** Succeeds if the next set of characters in the input is equal to the given string */
	def IsString(str:String):Parser[Unit] =
		parsers.IsString(str)

	/** A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree */
	def OfType[A](implicit tpe:ctx.TypeTag[A]):Parser[ctx.Expr[A]] =
		new parsers.OfType[ctx.type, A](tpe)

	/** A parser that succeeds if a "lift" type can be implicitly summoned
	 *
	 * The type of object to attempt to summon is determined by calling lifterType using the type of the next `arg` input
	 * The implicitly summoned value and the `arg` value are passed to `lift`; the returned value is returned by this parser
	 */
	def Lifted[Lifter[A], Z](lift:LiftFunction[ctx.type, Lifter, Z], description:String)(implicit lifterTypeTag:ctx.TypeTag[Lifter[_]]):Parser[Z] =
		parsers.Lifted(ctx)(lift, ExpectingDescription(description))

	/**
	 * A parser that consumes no input and always succeeds
	 * @group Constant
	 */
	def Pass:Parser[Unit] =
		parsers.Pass

	/**
	 * A parser that always reports a failure
	 * @group Constant
	 */
	def Fail(message:String):Parser[Nothing] =
		parsers.Fail(ExpectingDescription(message))

	/** A parser that succeeds iff the input is empty */
	def End:Parser[Unit] =
		new parsers.End()

	/** Indirectly refers to a parser, to allow for mutual-recursion */
	def DelayedConstruction[A](fn:Function0[Parser[A]]):Parser[A] =
		new parsers.DelayedConstruction(fn)
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
