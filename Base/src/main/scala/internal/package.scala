package name.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.{Seq, Set}

package internal {
	/** A parser that extracts a value from an input's parts, and returns None for all args */
	private[internal] final class PartsParser[Ctx, Expr[+_], Type[_], A](
		partsFn:String => Option[(A, Int)],
		expecting: ExpectingDescription
	) extends Parser[Ctx, Expr, Type, A] {
		override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
			input.consume(
				partsFn,
				_ => None,
				expecting
			)
		}

		override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, A]] = {
			input.consume(
				partsFn.andThen(_.map({case (_, charCount) => (exprs.empty, charCount)})),
				_ => None,
				expecting
			)
		}

	}
}

package object internal {

	private def escape(in:Char):String = escape(in.toInt)

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
		case _ => CodePoint.unsafe_apply(in).toString
	}

	/**
	 * Returns a string that describes which codepoints match the predicate
	 */
	private def describeCodepointPredicate(predicate:Int => Boolean, domainMax: Int):ExpectingDescription = {
		ExpectingDescription.delayed({
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

			if (builder.length > 4) {
				builder.substring(0, builder.length - 4)
			} else {
				"nothing"
			}
		})
	}

	/* * * Leaf parsers * * */

	/** Succeeds if the next character is a member of the given Set; captures that character */
	private[stringContextParserCombinator]
	def CharIn[Ctx, Expr[+_], Type[_]](
		chooseFrom:Set[Char]
	):Parser[Ctx, Expr, Type, Char] = CharWhere(
		chooseFrom.contains _,
		ExpectingDescription(chooseFrom.map(c => escape(c)).mkString("CharIn(\"", "", "\")"))
	)

	/** Succeeds if the next character is a member of the given Seq; captures that character */
	private[stringContextParserCombinator]
	def CharIn[Ctx, Expr[+_], Type[_]](
		chooseFrom:Seq[Char]
	):Parser[Ctx, Expr, Type, Char] = CharWhere(
		chooseFrom.contains _,
		ExpectingDescription(chooseFrom.map(c => escape(c)).mkString("CharIn(\"", "", "\")"))
	)

	/** Succeeds if the next character matches the given predicate; captures that character */
	private[stringContextParserCombinator]
	def CharWhere[Ctx, Expr[+_], Type[_]](
		predicate:Function1[Char, Boolean]
	):Parser[Ctx, Expr, Type, Char] = {
		val description = describeCodepointPredicate(c => predicate(c.toChar), Character.MAX_VALUE)
		CharWhere(
			predicate,
			description
		)
	}

	/** Succeeds if the next character matches the given predicate; captures that character */
	private[stringContextParserCombinator]
	def CharWhere[Ctx, Expr[+_], Type[_]](
		predicate:Function1[Char, Boolean],
		description: ExpectingDescription
	):Parser[Ctx, Expr, Type, Char] = new PartsParser(
		pt => Option((pt.charAt(0), 1)).filter(x => predicate(x._1)),
		description
	)

	/** Succeeds if the next codepoint is a member of the given string; captures that code point */
	private[stringContextParserCombinator]
	def CodePointIn[Ctx, Expr[+_], Type[_]](
		chooseFrom:String
	):Parser[Ctx, Expr, Type, CodePoint] = {
		def IntEqualsCodePoint(x:CodePoint) = new java.util.function.IntPredicate{def test(y:Int) = {y == x.intValue}}
		this.CodePointWhere(
			{(x:CodePoint) => chooseFrom.codePoints.anyMatch(IntEqualsCodePoint(x))},
			ExpectingDescription(chooseFrom.map(c => escape(c)).mkString("CodePointIn(\"", "", "\")"))
		)
	}

	/** Succeeds if the next codepoint is a member of the given Set; captures that code point */
	private[stringContextParserCombinator]
	def CodePointIn[Ctx, Expr[+_], Type[_]](
		chooseFrom:Set[CodePoint]
	):Parser[Ctx, Expr, Type, CodePoint] = {
		this.CodePointWhere(
			chooseFrom.contains _,
			ExpectingDescription(chooseFrom.map(c => escape(c.intValue)).mkString("CodePointIn(\"", "", "\")"))
		)
	}

	/** Succeeds if the next codepoint is a member of the given Seq; captures that code point */
	private[stringContextParserCombinator]
	def CodePointIn[Ctx, Expr[+_], Type[_]](
		chooseFrom:Seq[CodePoint]
	):Parser[Ctx, Expr, Type, CodePoint] = {
		this.CodePointWhere(
			chooseFrom.contains _,
			ExpectingDescription(chooseFrom.map(c => escape(c.intValue)).mkString("CodePointIn(\"", "", "\")"))
		)
	}

	/** Succeeds if the next codepoint matches the given predicate; captures that code point */
	private[stringContextParserCombinator]
	def CodePointWhere[Ctx, Expr[+_], Type[_]](
		predicate:Function1[CodePoint, Boolean]
	):Parser[Ctx, Expr, Type, CodePoint] = {
		val description = describeCodepointPredicate(c => predicate(CodePoint.unsafe_apply(c)), Character.MAX_CODE_POINT)
		CodePointWhere(
			predicate,
			description
		)
	}

	/** Succeeds if the next codepoint matches the given predicate; captures that code point */
	private[stringContextParserCombinator]
	def CodePointWhere[Ctx, Expr[+_], Type[_]](
		predicate:Function1[CodePoint, Boolean], description:ExpectingDescription
	):Parser[Ctx, Expr, Type, CodePoint] = new PartsParser(
		pt => Option((CodePoint.unsafe_apply(pt.codePointAt(0)), pt.offsetByCodePoints(0, 1))).filter(x => predicate(x._1)),
		description
	)

	/** Succeeds if the next set of characters in the input is equal to the given string */
	private[stringContextParserCombinator]
	def IsString[Ctx, Expr[+_], Type[_]](
		value:String
	):Parser[Ctx, Expr, Type, Unit] = new PartsParser(
		pt => Option(((), value.length())).filter(_ => pt.startsWith(value)),
		ExpectingDescription(value.map(c => escape(c)).mkString("\"", "", "\""))
	)

	/** Succeeds if the net character data matches the given regex; captures the matched string */
	private[stringContextParserCombinator]
	def Regex[Ctx, Expr[+_], Type[_]](
		reg:scala.util.matching.Regex
	):Parser[Ctx, Expr, Type, String] = new PartsParser(
		pt => reg.findPrefixMatchOf(pt).map(m => (m.matched, m.end - m.start)),
		ExpectingDescription("s/" + reg.toString + "/")
	)
}
