package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Seq
import scala.language.higherKinds
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/**
 * A mixin that provides Parser factory methods that conform to a particular `U` parameter.
 *
 * Exists to reduce the need to repeatedly explicitly specify `c.type` as the U parameter,
 * since the compiler seems to be not great at inferring a Singleton type
 *
 * And you'd need to put the parsers in an object anyway if you wanted to make recursive
 * parsers, so one is probably paying the cost required of the mixin anyway.
 */
trait Parsers {
	import com.rayrobdod.{stringContextParserCombinator => scpc}
	val ctx:Context
	type Parser[A] = scpc.Parser[ctx.type, A]
	/** Succeeds if the next character is a member of the given String; captures that character */
	def CharIn(str:Seq[Char]):Parser[Char] = parsers.CharIn[ctx.type](str)
	/** Succeeds if the next character is a member of the given Seq; captures that character */
	def CharIn(str:String):Parser[Char] = parsers.CharIn[ctx.type](scala.Predef.wrapString(str))
	/** Succeeds if the next character matches the given predicate; captures that character */
	def CharWhere(fn:Function1[Char, Boolean], description:String):Parser[Char] = parsers.CharWhere[ctx.type](fn, Failure.Leaf(description))
	/** Succeeds if the next codepoint is a member of the given string; captures that code point */
	def CodePointIn(str:String):Parser[CodePoint] = parsers.CodePointIn(str)
	/** Succeeds if the next codepoint is matches the given predicate; captures that code point */
	def CodePointWhere(fn:Function1[CodePoint, Boolean], description:String):Parser[CodePoint] = parsers.CodePointWhere(fn, Failure.Leaf(description))
	/** Succeeds if the next set of characters in the input is equal to the given string */
	def IsString(str:String):Parser[Unit] = parsers.IsString[ctx.type](str)
	/** A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree */
	def OfType[A](implicit tpe:ctx.TypeTag[A]):Parser[ctx.Expr[A]] = parsers.OfType[ctx.type, A](tpe)
	/** A parser that succeeds if a "lift" type can be implicitly summoned
	 *
	 * The type of object to attempt to summon is determined by calling lifterType using the type of the next `arg` input
	 * The implicitly summoned value and the `arg` value are passed to `lift`; the returned value is returned by this parser
	 */
	def Lifted[Lifter[A], Z](lifterType:Function1[ctx.Type, ctx.Type], lift:LiftFunction[ctx.type, Lifter, Z], description:Failure.Expecting):Parser[ctx.Expr[Z]] = parsers.Lifted(ctx)(lifterType, lift, description)
	/** A parser that succeeds iff the input is empty */
	def End():Parser[Unit] = parsers.End[ctx.type]()
	/** Indirectly refers to a parser, to allow for mutual-recursion */
	def DelayedConstruction[A](fn:Function0[Parser[A]]):Parser[A] = parsers.DelayedConstruction(fn)
}

/**
 * Methods to create leaf parsers
 */
object Parsers {
	/** Succeeds if the next character is a member of the given String; captures that character */
	def CharIn[U <: Context with Singleton](str:Seq[Char]):Parser[U, Char] = parsers.CharIn[U](str)
	/** Succeeds if the next character is a member of the given Seq; captures that character */
	def CharIn[U <: Context with Singleton](str:String):Parser[U, Char] = parsers.CharIn[U](scala.Predef.wrapString(str))
	/** Succeeds if the next character matches the given predicate; captures that character */
	def CharWhere[U <: Context with Singleton](fn:Function1[Char, Boolean], description:String):Parser[U, Char] = parsers.CharWhere[U](fn, Failure.Leaf(description))
	/** Succeeds if the next codepoint is a member of the given string; captures that code point */
	def CodePointIn[U <: Context with Singleton](str:String):Parser[U, CodePoint] = parsers.CodePointIn(str)
	/** Succeeds if the next codepoint is matches the given predicate; captures that code point */
	def CodePointWhere[U <: Context with Singleton](fn:Function1[CodePoint, Boolean], description:String):Parser[U, CodePoint] = parsers.CodePointWhere(fn, Failure.Leaf(description))
	/** Succeeds if the next set of characters in the input is equal to the given string */
	def IsString[U <: Context with Singleton](str:String):Parser[U, Unit] = parsers.IsString[U](str)
	/** A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree */
	def OfType[U <: Context with Singleton, A](implicit tpe:U#TypeTag[A]):Parser[U, U#Expr[A]] = parsers.OfType[U, A](tpe)
	/** A parser that succeeds if a "lift" type can be implicitly summoned
	 *
	 * The type of object to attempt to summon is determined by calling lifterType using the type of the next `arg` input
	 * The implicitly summoned value and the `arg` value are passed to `lift`; the returned value is returned by this parser
	 */
	def Lifted[Lifter[A], Z](c:Context)(lifterType:Function1[c.Type, c.Type], lift:LiftFunction[c.type, Lifter, Z], description:Failure.Expecting):Parser[c.type, c.Expr[Z]] = parsers.Lifted(c)(lifterType, lift, description)
	/** A parser that succeeds iff the input is empty */
	def End[U <: Context with Singleton]():Parser[U, Unit] = parsers.End[U]()
	/** Indirectly refers to a parser, to allow for mutual-recursion */
	def DelayedConstruction[U <: Context with Singleton, A](fn:Function0[Parser[U, A]]):Parser[U, A] = parsers.DelayedConstruction(fn)
}
