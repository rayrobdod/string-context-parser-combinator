package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.{Seq, Set}

package parsers {
	/** A parser that extracts a value from an input's parts, and returns None for all args */
	private[parsers] final class PartsParser[+A](
		partsFn:String => Option[(A, Int)],
		expecting: ExpectingDescription
	) extends Parser[Any, A] {
		def parse[ExprZ <: Any, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
			input.consume(
				partsFn,
				_ => None,
				expecting
			)
		}
	}
}

package object parsers {

	/**
	 * Returns a string representing the given code point, possibly represented
	 * with scala-type escape sequences
	 */
	private def escape(in:Int):String = in match {
		case '\"' => "\\\""
		case '\\' => "\\\\"
		case '\b' => "\\b"
		case '\f' => "\\f"
		case '\n' => "\\n"
		case '\r' => "\\r"
		case '\t' => "\\t"
		case x if Character.isISOControl(x) => f"\\u${x.toInt}%04X"
		case _ => CodePoint(in).toString
	}

	/**
	 * Returns a string that describes which codepoints match the predicate
	 */
	private def describeCodepointPredicate(predicate:Int => Boolean, domainMax: Int):ExpectingDescription = {
		val builder = new StringBuilder()
		var inMatchingBlock:Boolean = false
		var firstCharOfBlock:Int = 0

		(0 to domainMax).foreach({c =>
			if (predicate(c)) {
				if (inMatchingBlock) {
					// continue
				} else {
					inMatchingBlock = true
					firstCharOfBlock = c
				}
			} else {
				if (inMatchingBlock) {
					builder.++=("'")
					builder.++=(escape(firstCharOfBlock))
					if (firstCharOfBlock != c - 1) {
						builder.++=("'<=c<='")
						builder.++=(escape((c - 1)))
					}
					builder.++=("' or ")
					inMatchingBlock = false
				} else {
					// continue
				}
			}
		})
		if (inMatchingBlock) {
			builder.++=("'")
			builder.++=(escape(firstCharOfBlock))
			builder.++=("'<=c<='")
			builder.++=(escape(domainMax))
			builder.++=("' or ")
		}
		ExpectingDescription(
			if (builder.length > 4) {
				builder.substring(0, builder.length - 4)
			} else {
				"nothing"
			}
		)
	}

	/* * * Leaf parsers * * */

	/** Succeeds if the next character is a member of the given Set; captures that character */
	private[stringContextParserCombinator]
	def CharIn(
		chooseFrom:Set[Char]
	):Parser[Any, Char] = CharWhere(
		chooseFrom.contains _,
		ExpectingDescription(chooseFrom.map(c => escape(c)).mkString("CharIn(\"", "", "\")"))
	)

	/** Succeeds if the next character is a member of the given Seq; captures that character */
	private[stringContextParserCombinator]
	def CharIn(
		chooseFrom:Seq[Char]
	):Parser[Any, Char] = CharWhere(
		chooseFrom.contains _,
		ExpectingDescription(chooseFrom.map(c => escape(c)).mkString("CharIn(\"", "", "\")"))
	)

	/** Succeeds if the next character matches the given predicate; captures that character */
	private[stringContextParserCombinator]
	def CharWhere(
		predicate:Function1[Char, Boolean]
	):Parser[Any, Char] = {
		val description = describeCodepointPredicate(c => predicate(c.toChar), Character.MAX_VALUE)
		CharWhere(
			predicate,
			description
		)
	}

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
			ExpectingDescription(chooseFrom.map(c => escape(c)).mkString("CodePointIn(\"", "", "\")"))
		)
	}

	/** Succeeds if the next codepoint is a member of the given Set; captures that code point */
	private[stringContextParserCombinator]
	def CodePointIn(
		chooseFrom:Set[CodePoint]
	):Parser[Any, CodePoint] = {
		this.CodePointWhere(
			chooseFrom.contains _,
			ExpectingDescription(chooseFrom.map(c => escape(c.value)).mkString("CodePointIn(\"", "", "\")"))
		)
	}

	/** Succeeds if the next codepoint is a member of the given Seq; captures that code point */
	private[stringContextParserCombinator]
	def CodePointIn(
		chooseFrom:Seq[CodePoint]
	):Parser[Any, CodePoint] = {
		this.CodePointWhere(
			chooseFrom.contains _,
			ExpectingDescription(chooseFrom.map(c => escape(c.value)).mkString("CodePointIn(\"", "", "\")"))
		)
	}

	/** Succeeds if the next codepoint matches the given predicate; captures that code point */
	private[stringContextParserCombinator]
	def CodePointWhere(
		predicate:Function1[CodePoint, Boolean]
	):Parser[Any, CodePoint] = {
		val description = describeCodepointPredicate(c => predicate(CodePoint(c)), Character.MAX_CODE_POINT)
		CodePointWhere(
			predicate,
			description
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
		ExpectingDescription(value.map(c => escape(c)).mkString("\"", "", "\""))
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
		def parse[ExprZ <: Any, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Unit] = Success((), input, ExpectingSet.empty)
	}

	/** A parser that consumes no input and always fails */
	private[stringContextParserCombinator]
	def Fail(desc:ExpectingDescription):Parser[Any, Nothing] = new Parser[Any, Nothing] {
		def parse[ExprZ <: Any, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Nothing] = Failure(ExpectingSet(Expecting(desc, input.position)))
	}

	private[stringContextParserCombinator]
	def Optionally[Expr, A, Z](
		backing:Parser[Expr, A],
		strategy:RepeatStrategy,
		ev:typeclass.Optionally[A, Z]
	):Parser[Expr, Z] = {
		new Repeat(backing, 0, 1, Pass, strategy, new typeclass.Repeated[A, Z] {
			final class Box[BoxType](var value:BoxType)
			type Acc = Box[Z]
			def init():Acc = new Box(ev.none)
			def append(acc:Acc, elem:A):Unit = acc.value = ev.some(elem)
			def result(acc:Acc):Z = acc.value
		})
	}
}
