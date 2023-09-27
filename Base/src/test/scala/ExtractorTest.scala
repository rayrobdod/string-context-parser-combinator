package name.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Seq
import munit.Location

abstract class BaseExtractorSuite extends munit.FunSuite {
	val eof = ("" :: Nil, Nil)
	object InterpolatedValue {}

	def escape(in:Char):String = escape(in.toInt)

	/**
	 * Returns a string representing the given code point, possibly represented
	 * with scala-type escape sequences
	 */
	def escape(in:Int):String = in match {
		case '\"' => "\\\""
		case '\\' => "\\\\"
		case '\b' => "\\b"
		case '\f' => "\\f"
		case '\n' => "\\n"
		case '\r' => "\\r"
		case '\t' => "\\t"
		case x if
			x == '\uFFFF' ||
			Character.isISOControl(x) ||
			(x < 0x10000 && Character.isSurrogate(x.toChar)) ||
			false => f"\\u${x.toInt}%04X"
		case x if
			x >= 0x10000 ||
			false => f"\\U{${x.toInt}%X}"
		case _ => CodePoint(in).toString
	}

	def assertParseSuccess[A](
		dut:Extractor[Id, Class, A],
		input:(List[String], A),
		expecting:Option[Seq[Any]]
	)(implicit loc: Location
	):Unit = {
		val result = dut.extract(StringContext(input._1:_*), input._2)
		assertEquals(result, expecting)
	}

	def assertParseFailure[A](
		dut:Extractor[Id, Class, A],
		input:(List[String], A),
		messageLines:List[String]
	)(implicit loc: Location
	):Unit = {
		val expecting = messageLines.mkString("\n")
		val result = intercept[ParseException] {
			dut.extract(StringContext(input._1:_*), input._2)
		}
		assertEquals(result.getMessage(), expecting)
	}

	def assertParseFailureOnEmptyInput[A](
		dut:Extractor[Id, Class, A],
		scrutinee: A,
		expectingLine:String
	)(implicit loc: Location
	):Unit = {
		test ("throws when the input is empty") {
			assertParseFailure(dut, ("" :: Nil, scrutinee), List(expectingLine, "\t", "\t^"))
		}
	}

	def assertParseFailureOnExpr[A](
		dut:Extractor[Id, Class, A],
		scrutinee: A,
		expectingLine:String
	)(implicit loc: Location
	):Unit = {
		test ("throws when the next input is an expr") {
			assertParseFailure(dut, ("" :: "value" :: Nil, scrutinee), List(expectingLine, "\t${}value", "\t^"))
		}
	}

}

package ExtractorTest {
	package CharWhere {
		final class ConstFalse extends BaseExtractorSuite {
			val expectingLine = "Expected nothing"
			val dut = Extractor.idExtractors.charWhere(_ => false)
			val characterCases = List('\u0000', 'a', '\uFFFF', '\uD83D')

			assertParseFailureOnEmptyInput(dut, '1', expectingLine)
			assertParseFailureOnExpr(dut, '1', expectingLine)
			for (inputChar <- characterCases) {
				test (s"throws when the next input is the character '${escape(inputChar)}'") {
					assertParseFailure(dut, (s"$inputChar" :: Nil, '\u0000'), List(expectingLine, s"\t$inputChar", "\t^"))
				}
			}
		}
		final class ConstTrue extends BaseExtractorSuite {
			val expectingLine = "Expected '\\u0000'<=c<='\uFFFF'"
			val dut = Extractor.idExtractors.charWhere(_ => true)
			val characterCases = List('\u0000', 'a', '\uFFFF')

			assertParseFailureOnEmptyInput(dut, '1', expectingLine)
			assertParseFailureOnExpr(dut, '1', expectingLine)
			for (inputChar <- characterCases) {
				val scrutinee = '\u0000'
				val expecting = Some(Nil)
				test (s"returns ${expecting} when input is '${escape(inputChar)}' and scrutinee is '${escape(scrutinee)}'") {
					assertParseSuccess(dut, (s"$inputChar" :: Nil, scrutinee), expecting)
				}
			}
		}
		final class IsAsciiDigit extends BaseExtractorSuite {
			val expectingLine = "Expected '0'<=c<='9'"
			val dut = Extractor.idExtractors.charWhere(c => '0' <= c && c <= '9')
			val passCharCases = List('0', '9')
			val failCharCases = List[Char]('a', '0' - 1, '9' + 1, '\u0000', '\uFFFF')

			assertParseFailureOnEmptyInput(dut, '1', expectingLine)
			assertParseFailureOnExpr(dut, '1', expectingLine)
			for (inputChar <- passCharCases) {
				val scrutinee = '\u0000'
				val expecting = Some(Nil)
				test (s"returns ${expecting} when input is '${escape(inputChar)}' and scrutinee is '${escape(scrutinee)}'") {
					assertParseSuccess(dut, (s"$inputChar" :: Nil, scrutinee), expecting)
				}
			}
			for (inputChar <- failCharCases) {
				test (s"throws when the next input is the character '${escape(inputChar)}'") {
					assertParseFailure(dut, (s"$inputChar" :: Nil, '\u0000'), List(expectingLine, s"\t$inputChar", "\t^"))
				}
			}
		}
	}
	package CodePointWhere {
		final class ConstFalse extends BaseExtractorSuite {
			val expectingLine = "Expected nothing"
			val dut = Extractor.idExtractors.codePointWhere(_ => false)
			val characterCases = List('\u0000', 'a', '\uFFFF', '\uD83D')

			assertParseFailureOnEmptyInput(dut, CodePoint(0x31), expectingLine)
			assertParseFailureOnExpr(dut, CodePoint(0x31), expectingLine)
			for (inputChar <- characterCases) {
				test (s"throws when the next input is the character '${escape(inputChar)}'") {
					assertParseFailure(dut, (s"$inputChar" :: Nil, CodePoint(0)), List(expectingLine, s"\t$inputChar", "\t^"))
				}
			}
		}
		final class ConstTrue extends BaseExtractorSuite {
			val expectingLine = "Expected '\\u0000'<=c<='\uDBFF\uDFFF'"
			val dut = Extractor.idExtractors.codePointWhere(_ => true)
			val characterCases = List[Int](0, 'a', 0xFFFF, CodePoint.MaxValue)

			assertParseFailureOnEmptyInput(dut, CodePoint(0x31), expectingLine)
			assertParseFailureOnExpr(dut, CodePoint(0x31), expectingLine)
			for (inputChar <- characterCases) {
				val scrutinee = 0
				val expecting = Some(Nil)
				test (s"returns ${expecting} when input is '${escape(inputChar)}' and scrutinee is '${escape(scrutinee)}'") {
					assertParseSuccess(dut, (s"${CodePoint(inputChar)}" :: Nil, CodePoint(scrutinee)), expecting)
				}
			}
			test ("matches the entire surrogate pair") {
				assertParseSuccess(dut, ("\uD83D\uDE00" :: Nil, CodePoint(128512)), Some(Nil))
			}
		}
		final class IsPlayingCard extends BaseExtractorSuite {
			val expectingLine = "Expected '🂡'<=c<='🂮'"
			val dut = Extractor.idExtractors.codePointWhere(c => 127137 <= c.intValue && c.intValue <= 127150)
			val passCharCases = List[Int](127137, 127144, 127150)
			val failCharCases = List[Int](0, 127136, 127151, 0xFFFF, CodePoint.MaxValue)

			assertParseFailureOnEmptyInput(dut, CodePoint(0x31), expectingLine)
			assertParseFailureOnExpr(dut, CodePoint(0x31), expectingLine)
			for (inputChar <- passCharCases) {
				val scrutinee = 0
				val expecting = Some(Nil)
				test (s"returns ${expecting} when input is '${escape(inputChar)}' and scrutinee is '${escape(scrutinee)}'") {
					assertParseSuccess(dut, (s"${CodePoint(inputChar)}" :: Nil, CodePoint(scrutinee)), expecting)
				}
			}
			for (inputChar <- failCharCases) {
				test (s"throws when the next input is the character '${escape(inputChar)}'") {
					assertParseFailure(dut, (s"${CodePoint(inputChar)}" :: Nil, CodePoint(0)), List(expectingLine, s"\t${CodePoint(inputChar)}", "\t^"))
				}
			}
		}
	}
	final class End extends BaseExtractorSuite {
		val expectingLine = "Expected EOF"
		val dut = Extractor.idExtractors.end

		test ("does match an empty input") {
			assertParseSuccess(dut, ("" :: Nil, ()), Option(Nil))
		}
		test ("throws when the next value is a character") {
			assertParseFailure(dut, ("value" :: Nil, ()), List(expectingLine, "\tvalue", "\t^"))
		}
		assertParseFailureOnExpr(dut, (), expectingLine)
	}
	final class OfType extends BaseExtractorSuite {
		sealed trait Scrutinee
		object Scrutinee1 extends Scrutinee
		object Scrutinee2 extends Scrutinee

		val expectingLine = s"Expected OfType(${classOf[Scrutinee].getName})"
		val dut = Extractor.idExtractors.ofType[Scrutinee](classOf[Scrutinee])

		assertParseFailureOnEmptyInput(dut, Scrutinee2, expectingLine)
		test ("throws when the next value is a character") {
			assertParseFailure(dut, ("value" :: Nil, Scrutinee2), List(expectingLine, "\tvalue", "\t^"))
		}
		test ("Returns the scrutinee when the next input is an expr") {
			assertParseSuccess(dut, ("" :: "value" :: Nil, Scrutinee1), Option(List(Scrutinee1)))
		}
	}
	package contramap {
		final case class Foo(x:Int)
		final case class Bar(x:Int)

		final class BarToFoo extends BaseExtractorSuite {
			val dut = Extractor.idExtractors.ofType(classOf[Foo]).contramap[Bar](bar => new Foo(bar.x))

			test ("Returns the mapped Foo when the next input is an Expr and the scrutinee is a Bar") {
				assertParseSuccess(dut, ("" :: "" :: Nil, Bar(42)), Option(List(Foo(42))))
			}
		}
	}
	package widenWith {
		trait Superclass
		final case class Subclass1(x:Int) extends Superclass
		final case class Subclass2(x:Int) extends Superclass

		final class IsInstanceOf extends BaseExtractorSuite {
			val dut = Extractor.idExtractors.ofType(classOf[Subclass1])
					.widenWith[Id, Superclass](PartialExprFunction[Id, Superclass, Subclass1]({(x:Superclass) => x.isInstanceOf[Subclass1]}, {(x:Superclass) => x.asInstanceOf[Subclass1]}))

			test ("on match") {
				assertParseSuccess(dut, ("" :: "" :: Nil, Subclass1(42)), Option(List(Subclass1(42))))
			}
			test ("on not match") {
				assertParseSuccess(dut, ("" :: "" :: Nil, Subclass2(42)), Option.empty)
			}
		}
	}
	package andThen {
		final class DigitAndThenAlpha extends BaseExtractorSuite {
			val digitExpectingLine = "Expected '0'<=c<='9'"
			val alphaExpectingLine = "Expected 'a'<=c<='z'"
			val digitParser = Extractor.idExtractors.charWhere(c => '0' <= c && c <= '9')
			val alphaParser = Extractor.idExtractors.charWhere(c => 'a' <= c && c <= 'z')

			val dut:Extractor[Id, Class, (Char, Char)] = digitParser.andThen[Id, Class, Char, (Char, Char)](alphaParser)

			assertParseFailureOnEmptyInput(dut, ('\u0000', '\u0000'), digitExpectingLine)
			test ("throws when expression has only one char; points at EOF") {
				assertParseFailure(dut, ("0" :: Nil, ('\u0000', '\u0000')), List(alphaExpectingLine, "\t0", "\t ^"))
			}
			test ("passes when expression has two matching chars") {
				assertParseSuccess(dut, ("0a" :: Nil, ('\u0000', '\u0000')), Option(List()))
			}
			test ("throws when expression has incorrect first char; points at first char") {
				assertParseFailure(dut, ("~~" :: Nil, ('\u0000', '\u0000')), List(digitExpectingLine, "\t~~", "\t^"))
			}
			test ("throws when expression has incorrect second char; points at second char") {
				assertParseFailure(dut, ("0~" :: Nil, ('\u0000', '\u0000')), List(alphaExpectingLine, "\t0~", "\t ^"))
			}
		}
	}
	package object orElse {
		implicit def discriminatedContraEithered[A, B]:typeclass.ContraEithered[Id, A, B, scala.util.Either[A, B]] = {
			typeclass.ContraEithered[Id, A, B, scala.util.Either[A, B]](
				PartialExprFunction[Id, scala.util.Either[A, B], A](
					it => it.isLeft,
					it => it.swap.toOption.get
				),
				PartialExprFunction[Id, scala.util.Either[A, B], B](
					it => it.isRight,
					it => it.toOption.get
				)
			)
		}
	}
	package orElse {

		final class IsStringOrElseIsString extends BaseExtractorSuite {
			val left = Extractor.idExtractors.isString("left")
			val right = Extractor.idExtractors.isString("right")

			val dut:Extractor[Id, Class, Either[Unit, Unit]] = left.orElse[Id, Class, Unit, Either[Unit, Unit]](right)(discriminatedContraEithered)

			test ("when string is left and scrutinee is left, then matches") {
				assertParseSuccess(dut, ("left" :: Nil, Left(())), Option(List.empty))
			}
			test ("when string is left and scrutinee is right, then not matches") {
				assertParseSuccess(dut, ("left" :: Nil, Right(())), Option.empty)
			}
			test ("when string is right and scrutinee is left, then not matches") {
				assertParseSuccess(dut, ("right" :: Nil, Left(())), Option.empty)
			}
			test ("when string is right and scrutinee is right, then matches") {
				assertParseSuccess(dut, ("right" :: Nil, Right(())), Option(List.empty))
			}
			test ("throws if string is neither") {
				assertParseFailure(dut, ("neither" :: Nil, Left(())), List("Expected \"left\" or \"right\"", "\tneither", "\t^"))
			}
		}
		final class PartsThatMayConsumeInput extends BaseExtractorSuite {
			def charIn(s:String) = Extractor.idExtractors.charIn(s).contramap({(_:Unit) => ' '})

			val abcd = charIn("a").andThen[Id, Class, Unit, Unit](charIn("b")).andThen[Id, Class, Unit, Unit](charIn("c")).andThen[Id, Class, Unit, Unit](charIn("d"))
			val abzy = charIn("a").andThen[Id, Class, Unit, Unit](charIn("b")).andThen[Id, Class, Unit, Unit](charIn("z")).andThen[Id, Class, Unit, Unit](charIn("y"))
			val dut:Extractor[Id, Class, Either[Unit, Unit]] = abcd.orElse(abzy)(discriminatedContraEithered)

			test ("even if left branch consumes input, then backtracking") {
				assertParseSuccess(dut, ("abzy" :: Nil, Right(())), Option(List()))
			}
		}
	}
	package repeat {
		import name.rayrobdod.stringContextParserCombinator.RepeatStrategy._

		final class ZeroOrMore extends BaseExtractorSuite {
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val dut:Extractor[Id, Class, List[String]] = one.repeat[Id, Class, List[String]](strategy = Greedy)

			for (inputLength <- 0 to 2; scrutineeLength <- 0 to (inputLength + 1)) {
				val input = List.fill(1 + inputLength)("")
				val scrutinee = List("a", "b", "c", "d", "e").take(scrutineeLength)

				if (inputLength == scrutineeLength) {
					test (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then matches") {
						assertParseSuccess(dut, (input, scrutinee), Option(scrutinee))
					}
				} else {
					test (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then not matches") {
						assertParseSuccess(dut, (input, scrutinee), Option.empty)
					}
				}
			}

			test ("will backtrack") {
				val dut2:Extractor[Id, Class, (List[String], String)] = dut.andThen[Id, Class, String, (List[String], String)](one)
				val input = List.fill(5)("")
				val scrutinee = (List("a", "b", "c"), "d")
				val expecting = List("a", "b", "c", "d")

				assertParseSuccess(dut2, (input, scrutinee), Option(expecting))
			}
		}
		final class ZeroOrMorePossessive extends BaseExtractorSuite {
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val dut:Extractor[Id, Class, List[String]] = one.repeat[Id, Class, List[String]](strategy = Possessive)

			for (inputLength <- 0 to 2; scrutineeLength <- 0 to (inputLength + 1)) {
				val input = List.fill(1 + inputLength)("")
				val scrutinee = List("a", "b", "c", "d", "e").take(scrutineeLength)

				if (inputLength == scrutineeLength) {
					test (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then matches") {
						assertParseSuccess(dut, (input, scrutinee), Option(scrutinee))
					}
				} else {
					test (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then not matches") {
						assertParseSuccess(dut, (input, scrutinee), Option.empty)
					}
				}
			}

			test ("will not backtrack") {
				val dut2:Extractor[Id, Class, (List[String], String)] = dut.andThen[Id, Class, String, (List[String], String)](one)
				val input = List.fill(4)("")
				val scrutinee = (List("a", "b", "c"), "d")

				assertParseFailure(dut2, (input, scrutinee), List("Expected OfType(java.lang.String)", "\t${}${}${}", "\t         ^"))
			}
		}
		final class ZeroOrMoreLazy extends BaseExtractorSuite {
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val dut:Extractor[Id, Class, List[String]] = one.repeat[Id, Class, List[String]](strategy = Lazy)

			for (inputLength <- 0 to 2; scrutineeLength <- 0 to (inputLength + 1)) {
				val input = List.fill(1 + inputLength)("")
				val scrutinee = List("a", "b", "c", "d", "e").take(scrutineeLength)

				if (0 == scrutineeLength) {
					test (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then matches") {
						assertParseSuccess(dut, (input, scrutinee), Option(Nil))
					}
				} else {
					test (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then not matches") {
						assertParseSuccess(dut, (input, scrutinee), Option.empty)
					}
				}
			}

			test ("will backtrack") {
				val dut2:Extractor[Id, Class, List[String]] = dut.andThen[Id, Class, Unit, List[String]](Extractor.idExtractors.end)
				val input = List.fill(4)("")
				val scrutinee = List("a", "b", "c")
				val expecting = List("a", "b", "c")

				assertParseSuccess(dut2, (input, scrutinee), Option(expecting))
			}
		}
		final class OneOrMore extends BaseExtractorSuite {
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val dut:Extractor[Id, Class, List[String]] = one.repeat[Id, Class, List[String]](1, strategy = Greedy)

			test (s"when input has length 0 then throws") {
				assertParseFailure(dut, ("" :: Nil, List()), List("Expected OfType(java.lang.String)", "\t", "\t^"))
			}

			for (inputLength <- 1 to 2; scrutineeLength <- 0 to (inputLength + 1)) {
				val input = List.fill(1 + inputLength)("")
				val scrutinee = List("a", "b", "c", "d", "e").take(scrutineeLength)

				if (inputLength == scrutineeLength) {
					test (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then matches") {
						assertParseSuccess(dut, (input, scrutinee), Option(scrutinee))
					}
				} else {
					test (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then not matches") {
						assertParseSuccess(dut, (input, scrutinee), Option.empty)
					}
				}
			}
		}
		final class ZeroOrOne extends BaseExtractorSuite {
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val dut:Extractor[Id, Class, List[String]] = one.repeat[Id, Class, List[String]](0, 1, strategy = Greedy)

			for (inputLength <- 0 to 2; scrutineeLength <- 0 to (inputLength + 1)) {
				val input = List.fill(1 + inputLength)("")
				val scrutinee = List("a", "b", "c", "d", "e").take(scrutineeLength)

				if (if (inputLength == 0) {scrutineeLength == 0} else {scrutineeLength == 1}) {
					test (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then matches") {
						assertParseSuccess(dut, (input, scrutinee), Option(scrutinee))
					}
				} else {
					test (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then not matches") {
						assertParseSuccess(dut, (input, scrutinee), Option.empty)
					}
				}
			}
		}
		final class PassRepeated extends BaseExtractorSuite {
			val one = Extractor.idExtractors.pass
			val dut = one.repeat[Id, Class, Unit](strategy = Greedy)

			test ("does not hang indefinitely") {
				val input = "413" :: Nil

				assertParseSuccess(dut, (input, ()), Option(Nil))
			}
		}
		final class RepeatedRepeated extends BaseExtractorSuite {
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val two:Extractor[Id, Class, List[String]] = one.repeat[Id, Class, List[String]](strategy = Greedy)
			val dut:Extractor[Id, Class, List[List[String]]] = two.repeat(strategy = Greedy)

			test ("does not hang indefinitely") {
				val input = "" :: "" :: Nil
				val scrutinee = List(List("a"))

				assertParseSuccess(dut, (input, scrutinee), Option.empty)
			}
		}
		final class ZeroOrMoreWithDelimiter extends BaseExtractorSuite {
			val delim = Extractor.idExtractors.isString(",")
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val dut:Extractor[Id, Class, List[String]] = one.repeat[Id, Class, List[String]](delimiter = delim, strategy = Greedy)

			for (inputLength <- 0 to 1; scrutineeLength <- 0 to (inputLength + 1)) {
				val input = List.fill(inputLength + 1)("")
				val scrutinee = List("a", "b", "c", "d", "e").take(scrutineeLength)

				if (inputLength == scrutineeLength) {
					test (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then matches") {
						assertParseSuccess(dut, (input, scrutinee), Option(scrutinee))
					}
				} else {
					test (s"when input has length ${inputLength} with scrutinee has length ${scrutineeLength}, then not matches") {
						assertParseSuccess(dut, (input, scrutinee), Option.empty)
					}
				}
			}
			{
				val inputLength = 5
				val inputWithDelim = "" :: "," :: "," :: "," :: "," :: "" :: Nil
				val inputSansDelim = "" :: ""  :: ""  :: ""  :: ""  :: "" :: Nil
				val letters = List("a", "b", "c", "d", "e", "f", "g", "h")

				test (s"when input has length ${inputLength} with delims and scrutinee has length ${inputLength}, then matches") {
					val scrutinee = letters.take(inputLength)
					assertParseSuccess(dut, (inputWithDelim, scrutinee), Option(scrutinee))
				}
				test (s"when input has length ${inputLength} sans delims, then pattern only matches one input") {
					val scrutinee = letters.take(1)
					assertParseSuccess(dut, (inputSansDelim, scrutinee), Option(scrutinee))
				}
			}
		}
		final class TwoOrMoreWithDelimiter extends BaseExtractorSuite {
			val delim = Extractor.idExtractors.isString(",")
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val dut:Extractor[Id, Class, List[String]] = one.repeat[Id, Class, List[String]](2, delimiter = delim, strategy = Greedy)

			test (s"when input has length 1, throws and reports expecting the delimiter") {
				val input = "" :: "" :: Nil
				val scrutinee = "a" :: Nil
				assertParseFailure(dut, (input, scrutinee), List("Expected \",\"", "\t${}", "\t   ^"))
			}
		}
		final class SequenceRepeated extends BaseExtractorSuite {
			def charIn(s:String) = Extractor.idExtractors.charIn(s)

			val one:Extractor[Id, Class, (Char, Char)] = charIn("a").andThen[Id, Class, Char, (Char, Char)](charIn("b"))
			val two:Extractor[Id, Class, ((Char, Char), Char)] = one.andThen[Id, Class, Char, ((Char, Char), Char)](charIn("c"))
			val dut:Extractor[Id, Class, List[((Char, Char), Char)]] = two.repeat[Id, Class, List[((Char, Char), Char)]](strategy = Greedy)
			val chartriple = (('\u0000', '\u0000'), '\u0000')

			test ("matches \"\"") {
					assertParseSuccess(dut, ("" :: Nil, Nil), Option(Nil))
			}
			test ("does not match \"a\"") {
					assertParseFailure(dut, ("a" :: Nil, Nil), List("Expected CharIn(\"b\")", "\ta", "\t ^"))
			}
			test ("does match \"abcde\"") {
					assertParseSuccess(dut, ("abcde" :: Nil, chartriple :: Nil), Option(Nil))
			}
			test ("does not match \"abca\"") {
					assertParseFailure(dut, ("abca" :: Nil, chartriple :: Nil), List("Expected CharIn(\"b\")", "\tabca", "\t    ^"))
			}
		}
	}
	package optionally {
		import name.rayrobdod.stringContextParserCombinator.RepeatStrategy._

		final class ZeroOrOne extends BaseExtractorSuite {
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val dut:Extractor[Id, Class, Option[String]] = one.optionally(strategy = Greedy)

			test (s"when input has length 0 and scrutinee is None, then matches") {
				assertParseSuccess(dut, ("" :: Nil, None), Option(Nil))
			}
			test (s"when input has length 1 and scrutinee is None, then not matches") {
				assertParseSuccess(dut, ("" :: "" :: Nil, None), None)
			}
			test (s"when input has length 0 and scrutinee is Some, then not matches") {
				assertParseSuccess(dut, ("" :: Nil, Some("Some")), None)
			}
			test (s"when input has length 1 and scrutinee is Some, then matches") {
				assertParseSuccess(dut, ("" :: "" :: Nil, Some("Some")), Option(List("Some")))
			}
		}
	}
}
