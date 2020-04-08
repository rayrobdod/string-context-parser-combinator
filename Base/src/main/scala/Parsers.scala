package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Seq
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
	type ContextType <: Context with Singleton
	type Parser[A] = scpc.Parser[ContextType, A]
	/** Succeeds if the next character is a member of the given String; captures that character */
	def CharIn(str:Seq[Char]):Parser[Char] = parsers.CharIn[ContextType](str)
	/** Succeeds if the next character is a member of the given Seq; captures that character */
	def CharIn(str:String):Parser[Char] = parsers.CharIn[ContextType](scala.Predef.wrapString(str))
	/** Succeeds if the next character matches the given predicate; captures that character */
	def CharWhere(fn:Function1[Char, Boolean], description:String):Parser[Char] = parsers.CharWhere[ContextType](fn, Failure.Leaf(description))
	/** Succeeds if the next codepoint is a member of the given string; captures that code point */
	def CodePointIn(str:String):Parser[CodePoint] = parsers.CodePointIn(str)
	/** Succeeds if the next codepoint is matches the given predicate; captures that code point */
	def CodePointWhere(fn:Function1[CodePoint, Boolean], description:String):Parser[CodePoint] = parsers.CodePointWhere(fn, Failure.Leaf(description))
	/** Succeeds if the next set of characters in the input is equal to the given string */
	def IsString(str:String):Parser[Unit] = parsers.IsString[ContextType](str)
	/** A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree */
	def OfType[A](tpe:ContextType#TypeTag[A]):Parser[ContextType#Expr[A]] = parsers.OfType[ContextType, A](tpe)
	/** A parser that succeeds iff the input is empty */
	def End():Parser[Unit] = parsers.End[ContextType]()
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
	def OfType[U <: Context with Singleton, A](tpe:U#TypeTag[A]):Parser[U, U#Expr[A]] = parsers.OfType[U, A](tpe)
	/** A parser that succeeds iff the input is empty */
	def End[U <: Context with Singleton]():Parser[U, Unit] = parsers.End[U]()
	/** Indirectly refers to a parser, to allow for mutual-recursion */
	def DelayedConstruction[U <: Context with Singleton, A](fn:Function0[Parser[U, A]]):Parser[U, A] = parsers.DelayedConstruction(fn)
}
