package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.{Seq, Set}
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

package parsers {
	/** An intermediary class to lessen the weight of implementing Parser repeatedly, Parser being a trait with several concrete methods */
	private[parsers] abstract class AbstractParser[Expr, +A] extends Parser[Expr,A]

	/** A parser that extracts a value from an input's parts, and returns None for all args */
	private[parsers] final class PartsParser[Expr, +A](
		partsFn:String => Option[(A, Int)],
		expecting: => Expecting
	) extends AbstractParser[Expr, A] {
		def parse(input:Input[Expr]):Result[Expr, A] = {
			input.consume(
				partsFn,
				_ => None,
				expecting
			)
		}
	}
}

package object parsers {
	/* * * Leaf parsers * * */

	/** Succeeds if the next character is a member of the given Set; captures that character */
	private[stringContextParserCombinator]
	def CharIn[Expr](
		chooseFrom:Set[Char]
	):Parser[Expr, Char] = CharWhere(
		chooseFrom.contains _,
		Expecting(chooseFrom.mkString("CharIn(\"", "", "\")"))
	)

	/** Succeeds if the next character is a member of the given Seq; captures that character */
	private[stringContextParserCombinator]
	def CharIn[Expr](
		chooseFrom:Seq[Char]
	):Parser[Expr, Char] = CharWhere(
		chooseFrom.contains _,
		Expecting(chooseFrom.mkString("CharIn(\"", "", "\")"))
	)

	/** Succeeds if the next character matches the given predicate; captures that character */
	private[stringContextParserCombinator]
	def CharWhere[Expr](
		predicate:Function1[Char, Boolean],
		description: => Expecting
	):Parser[Expr, Char] = new PartsParser(
		pt => Option((pt.charAt(0), 1)).filter(x => predicate(x._1)),
		description
	)

	/** Succeeds if the next codepoint is a member of the given string; captures that code point */
	private[stringContextParserCombinator]
	def CodePointIn[Expr](
		chooseFrom:String
	):Parser[Expr, CodePoint] = {
		def IntEqualsCodePoint(x:CodePoint) = new java.util.function.IntPredicate{def test(y:Int) = {y == x.value}}
		this.CodePointWhere(
			{x:CodePoint => chooseFrom.codePoints.anyMatch(IntEqualsCodePoint(x))},
			Expecting("CodePointIn(\"" + chooseFrom + "\")")
		)
	}

	/** Succeeds if the next codepoint is a member of the given Set; captures that code point */
	private[stringContextParserCombinator]
	def CodePointIn[Expr](
		chooseFrom:Set[CodePoint]
	):Parser[Expr, CodePoint] = {
		this.CodePointWhere(
			chooseFrom.contains _,
			Expecting(chooseFrom.mkString("CodePointIn(\"", "", "\")"))
		)
	}

	/** Succeeds if the next codepoint is a member of the given Seq; captures that code point */
	private[stringContextParserCombinator]
	def CodePointIn[Expr](
		chooseFrom:Seq[CodePoint]
	):Parser[Expr, CodePoint] = {
		this.CodePointWhere(
			chooseFrom.contains _,
			Expecting(chooseFrom.mkString("CodePointIn(\"", "", "\")"))
		)
	}

	/** Succeeds if the next codepoint matches the given predicate; captures that code point */
	private[stringContextParserCombinator]
	def CodePointWhere[Expr](
		predicate:Function1[CodePoint, Boolean], description:Expecting
	):Parser[Expr, CodePoint] = new PartsParser(
		pt => Option((CodePoint(pt.codePointAt(0)), pt.offsetByCodePoints(0, 1))).filter(x => predicate(x._1)),
		description
	)

	/** Succeeds if the next set of characters in the input is equal to the given string */
	private[stringContextParserCombinator]
	def IsString[Expr](
		value:String
	):Parser[Expr, Unit] = new PartsParser(
		pt => Option(((), value.length())).filter(_ => pt.startsWith(value)),
		Expecting("\"" + value + "\"")
	)

	/** Succeeds if the net character data matches the given regex; captures the matched string */
	private[stringContextParserCombinator]
	def Regex[Expr](
		reg:scala.util.matching.Regex
	):Parser[Expr, String] = new PartsParser(
		pt => reg.findPrefixMatchOf(pt).map(m => (m.matched, m.end - m.start)),
		Expecting("s/" + reg.toString + "/")
	)

	/** Succeeds if the next input element is an `arg` with the given type; captures the expression */
	private[stringContextParserCombinator]
	def OfType[Ctx <: Context with Singleton, A](
		tpetag:Ctx#TypeTag[A]
	):Parser[Ctx#Expr[_], Ctx#Expr[A]] = {
		new OfType(tpetag)
	}

	/** Succeeds only at the end of the given input */
	private[stringContextParserCombinator]
	def End[Expr](
	):Parser[Expr, Unit] = {
		new End()
	}

	private[stringContextParserCombinator]
	def NilParser[Expr]:Parser[Expr, Unit] = new Parser[Expr, Unit] {
		def parse(input:Input[Expr]):Result[Expr, Unit] = Success((), input, EmptyTrace(input), Cut.False)
	}

	/* * * Mapping * * */

	private[stringContextParserCombinator]
	def Map[Expr, A, Z](
		backing:Parser[Expr,A], mapping:Function1[A, Z]
	):Parser[Expr, Z] = {
		new Map(backing, mapping)
	}

	private[stringContextParserCombinator]
	def FlatMap[Expr, A, Z](
		backing:Parser[Expr, A], mapping:Function1[A, Parser[Expr, Z]]
	):Parser[Expr, Z] = {
		new FlatMap(backing, mapping)
	}

	private[stringContextParserCombinator]
	def Filter[Expr, A](
		backing:Parser[Expr, A], predicate:Function1[A, Boolean], description:Expecting
	):Parser[Expr, A] = {
		new Filter(backing, predicate, description)
	}

	private[stringContextParserCombinator]
	def Opaque[Expr, A](
		backing:Parser[Expr, A], description:Expecting
	):Parser[Expr, A] = {
		new Opaque(backing, description)
	}

	/** Used to allow mutually recursive parsers */
	private[stringContextParserCombinator]
	def DelayedConstruction[Expr, A](
		backing:Function0[Parser[Expr, A]]
	):Parser[Expr, A] = {
		new DelayedConstruction(backing)
	}

	private[stringContextParserCombinator]
	def Repeat[Expr, A, Z](
		backing:Parser[Expr, A],
		min:Int,
		max:Int,
		delimiter:Parser[Expr, Unit],
		ev:typelevel.Repeated[A, Z]
	):Parser[Expr, Z] = {
		new Repeat(backing, min, max, delimiter, ev)
	}

	private[stringContextParserCombinator]
	def Optionally[Expr, A, Z](
		backing:Parser[Expr, A], ev:typelevel.Optionally[A, Z]
	):Parser[Expr, Z] = {
		new Repeat(backing, 0, 1, NilParser, new typelevel.Repeated[A, Z] {
			final class Box[BoxType](var value:BoxType)
			type Acc = Box[Z]
			def init():Acc = new Box(ev.none())
			def append(acc:Acc, elem:A):Unit = acc.value = ev.some(elem)
			def result(acc:Acc):Z = acc.value
		})
	}

	/* * * Combinations * * */

	private[stringContextParserCombinator]
	def AndThen[Expr, A, B, Z](
		left:Parser[Expr, A], right:Parser[Expr, B], ev:typelevel.Sequenced[A, B, Z]
	):Parser[Expr, Z] = {
		new AndThen(left, right, ev)
	}

	private[stringContextParserCombinator]
	def AndThenWithCut[Expr, A, B, Z](
		left:Parser[Expr, A], right:Parser[Expr, B], ev:typelevel.Sequenced[A, B, Z]
	):Parser[Expr, Z] = {
		new AndThenWithCut(left, right, ev)
	}

	private[stringContextParserCombinator]
	def OrElse[Expr, A](
		left:Parser[Expr, A], right:Parser[Expr, A]
	):Parser[Expr, A] = {
		new OrElse(left, right)
	}
}
