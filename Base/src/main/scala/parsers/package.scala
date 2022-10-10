package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.{Seq, Set}

package parsers {
	/** A parser that extracts a value from an input's parts, and returns None for all args */
	private[parsers] final class PartsParser[+A](
		partsFn:String => Option[(A, Int)],
		expecting: ExpectingDescription
	) extends Parser[Any, A] {
		def parse[ExprZ <: Any, Pos](input:Input[ExprZ, Pos]):Result[ExprZ, Pos, A] = {
			input.consume(
				partsFn,
				_ => None,
				expecting
			)
		}
	}
}

package object parsers {

	private def unescape(in:Char):String = in match {
		case '\"' => "\\\""
		case '\\' => "\\\\"
		case '\b' => "\\b"
		case '\f' => "\\f"
		case '\n' => "\\n"
		case '\r' => "\\r"
		case '\t' => "\\t"
		case x if x < ' ' => f"\\u${x.toInt}%04d"
		case x => f"$x"
	}

	private def unescape(in:CodePoint):String = in.value match {
		case '\"' => "\\\""
		case '\\' => "\\\\"
		case '\b' => "\\b"
		case '\f' => "\\f"
		case '\n' => "\\n"
		case '\r' => "\\r"
		case '\t' => "\\t"
		case x if x < ' ' => f"\\u${x}%04d"
		case _ => in.toString
	}

	/* * * Leaf parsers * * */

	/** Succeeds if the next character is a member of the given Set; captures that character */
	private[stringContextParserCombinator]
	def CharIn(
		chooseFrom:Set[Char]
	):Parser[Any, Char] = CharWhere(
		chooseFrom.contains _,
		ExpectingDescription(chooseFrom.map(unescape _).mkString("CharIn(\"", "", "\")"))
	)

	/** Succeeds if the next character is a member of the given Seq; captures that character */
	private[stringContextParserCombinator]
	def CharIn(
		chooseFrom:Seq[Char]
	):Parser[Any, Char] = CharWhere(
		chooseFrom.contains _,
		ExpectingDescription(chooseFrom.map(unescape _).mkString("CharIn(\"", "", "\")"))
	)

	/** Succeeds if the next character matches the given predicate; captures that character */
	private[stringContextParserCombinator]
	def CharWhere(
		predicate:Function1[Char, Boolean],
		description: ExpectingDescription
	):Parser[Any, Char] = new PartsParser(
		pt => Option((pt.charAt(0), 1)).filter(x => predicate(x._1)),
		description
	)

	/** Succeeds if the next codepoint is a member of the given string; captures that code point */
	private[stringContextParserCombinator]
	def CodePointIn(
		chooseFrom:String
	):Parser[Any, CodePoint] = {
		def IntEqualsCodePoint(x:CodePoint) = new java.util.function.IntPredicate{def test(y:Int) = {y == x.value}}
		this.CodePointWhere(
			{(x:CodePoint) => chooseFrom.codePoints.anyMatch(IntEqualsCodePoint(x))},
			ExpectingDescription(chooseFrom.map(unescape _).mkString("CodePointIn(\"", "", "\")"))
		)
	}

	/** Succeeds if the next codepoint is a member of the given Set; captures that code point */
	private[stringContextParserCombinator]
	def CodePointIn(
		chooseFrom:Set[CodePoint]
	):Parser[Any, CodePoint] = {
		this.CodePointWhere(
			chooseFrom.contains _,
			ExpectingDescription(chooseFrom.map(unescape _).mkString("CodePointIn(\"", "", "\")"))
		)
	}

	/** Succeeds if the next codepoint is a member of the given Seq; captures that code point */
	private[stringContextParserCombinator]
	def CodePointIn(
		chooseFrom:Seq[CodePoint]
	):Parser[Any, CodePoint] = {
		this.CodePointWhere(
			chooseFrom.contains _,
			ExpectingDescription(chooseFrom.map(unescape _).mkString("CodePointIn(\"", "", "\")"))
		)
	}

	/** Succeeds if the next codepoint matches the given predicate; captures that code point */
	private[stringContextParserCombinator]
	def CodePointWhere(
		predicate:Function1[CodePoint, Boolean], description:ExpectingDescription
	):Parser[Any, CodePoint] = new PartsParser(
		pt => Option((CodePoint(pt.codePointAt(0)), pt.offsetByCodePoints(0, 1))).filter(x => predicate(x._1)),
		description
	)

	/** Succeeds if the next set of characters in the input is equal to the given string */
	private[stringContextParserCombinator]
	def IsString(
		value:String
	):Parser[Any, Unit] = new PartsParser(
		pt => Option(((), value.length())).filter(_ => pt.startsWith(value)),
		ExpectingDescription(value.map(unescape _).mkString("\"", "", "\""))
	)

	/** Succeeds if the net character data matches the given regex; captures the matched string */
	private[stringContextParserCombinator]
	def Regex(
		reg:scala.util.matching.Regex
	):Parser[Any, String] = new PartsParser(
		pt => reg.findPrefixMatchOf(pt).map(m => (m.matched, m.end - m.start)),
		ExpectingDescription("s/" + reg.toString + "/")
	)

	/** A parser that consumes no input and always succeeds */
	private[stringContextParserCombinator]
	def Pass:Parser[Any, Unit] = new Parser[Any, Unit] {
		def parse[ExprZ <: Any, Pos](input:Input[ExprZ, Pos]):Result[ExprZ, Pos, Unit] = Success((), input, Set.empty, Cut.False)
	}

	/** A parser that consumes no input and always fails */
	private[stringContextParserCombinator]
	def Fail(desc:ExpectingDescription):Parser[Any, Nothing] = new Parser[Any, Nothing] {
		def parse[ExprZ <: Any, Pos](input:Input[ExprZ, Pos]):Result[ExprZ, Pos, Nothing] = Failure(Set(Expecting(desc, input.position)), Cut.False)
	}

	private[stringContextParserCombinator]
	def Optionally[Expr, A, Z](
		backing:Parser[Expr, A],
		strategy:RepeatStrategy,
		ev:typelevel.Optionally[A, Z]
	):Parser[Expr, Z] = {
		new Repeat(backing, 0, 1, Pass, strategy, new typelevel.Repeated[A, Z] {
			final class Box[BoxType](var value:BoxType)
			type Acc = Box[Z]
			def init():Acc = new Box(ev.none)
			def append(acc:Acc, elem:A):Unit = acc.value = ev.some(elem)
			def result(acc:Acc):Z = acc.value
		})
	}
}
