package com.rayrobdod.stringContextParserCombinator

import org.scalatest.funspec.AnyFunSpec

final class ExtractorTest extends AnyFunSpec {
	val eof = ("" :: Nil, Nil)
	object InterpolatedValue {}

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
	):Unit = {
		val result = dut.extract(StringContext(input._1:_*), input._2)
		assertResult(expecting)(result)
	}

	def assertParseFailure[A](
		dut:Extractor[Id, Class, A],
		input:(List[String], A),
		messageLines:List[String]
	):Unit = {
		val expecting = messageLines.mkString("\n")
		val result = intercept[ParseException] {
			dut.extract(StringContext(input._1:_*), input._2)
		}
		assertResult(expecting)(result.getMessage())
	}

	def assertParseFailureOnEmptyInput[A](
		dut:Extractor[Id, Class, A],
		scrutinee: A,
		expectingLine:String
	):Unit = {
		it ("throws when the input is empty") {
			assertParseFailure(dut, ("" :: Nil, scrutinee), List(expectingLine, "\t", "\t^"))
		}
	}

	def assertParseFailureOnExpr[A](
		dut:Extractor[Id, Class, A],
		scrutinee: A,
		expectingLine:String
	):Unit = {
		it ("throws when the next input is an expr") {
			assertParseFailure(dut, ("" :: "value" :: Nil, scrutinee), List(expectingLine, "\t${}value", "\t^"))
		}
	}

	describe("Extractor.idExtractors.charWhere") {
		describe("Extractor.idExtractors.charWhere(_ => false)") {
			val expectingLine = "Expected nothing"
			val dut = Extractor.idExtractors.charWhere(_ => false)
			val characterCases = List('\u0000', 'a', '\uFFFF', '\uD83D')

			assertParseFailureOnEmptyInput(dut, '1', expectingLine)
			assertParseFailureOnExpr(dut, '1', expectingLine)
			for (inputChar <- characterCases) {
				it (s"throws when the next input is the character '${escape(inputChar)}'") {
					assertParseFailure(dut, (s"$inputChar" :: Nil, '\u0000'), List(expectingLine, s"\t$inputChar", "\t^"))
				}
			}
		}
		describe("Extractor.idExtractors.charWhere(_ => true)") {
			val expectingLine = "Expected '\\u0000'<=c<='\uFFFF'"
			val dut = Extractor.idExtractors.charWhere(_ => true)
			val characterCases = List('\u0000', 'a', '\uFFFF')

			assertParseFailureOnEmptyInput(dut, '1', expectingLine)
			assertParseFailureOnExpr(dut, '1', expectingLine)
			for (inputChar <- characterCases) {
				val scrutinee = '\u0000'
				val expecting = Some(Nil)
				it (s"returns ${expecting} when input is '${escape(inputChar)}' and scrutinee is '${escape(scrutinee)}'") {
					assertParseSuccess(dut, (s"$inputChar" :: Nil, scrutinee), expecting)
				}
			}
		}
		describe("Extractor.idExtractors.charWhere(c => '0' <= c && c <= '9')") {
			val expectingLine = "Expected '0'<=c<='9'"
			val dut = Extractor.idExtractors.charWhere(c => '0' <= c && c <= '9')
			val passCharCases = List('0', '9')
			val failCharCases = List[Char]('a', '0' - 1, '9' + 1, '\u0000', '\uFFFF')

			assertParseFailureOnEmptyInput(dut, '1', expectingLine)
			assertParseFailureOnExpr(dut, '1', expectingLine)
			for (inputChar <- passCharCases) {
				val scrutinee = '\u0000'
				val expecting = Some(Nil)
				it (s"returns ${expecting} when input is '${escape(inputChar)}' and scrutinee is '${escape(scrutinee)}'") {
					assertParseSuccess(dut, (s"$inputChar" :: Nil, scrutinee), expecting)
				}
			}
			for (inputChar <- failCharCases) {
				it (s"throws when the next input is the character '${escape(inputChar)}'") {
					assertParseFailure(dut, (s"$inputChar" :: Nil, '\u0000'), List(expectingLine, s"\t$inputChar", "\t^"))
				}
			}
		}
	}
	describe("Extractor.idExtractors.codePointWhere") {
		describe("Extractor.idExtractors.codePointWhere(_ => false)") {
			val expectingLine = "Expected nothing"
			val dut = Extractor.idExtractors.codePointWhere(_ => false)
			val characterCases = List('\u0000', 'a', '\uFFFF', '\uD83D')

			assertParseFailureOnEmptyInput(dut, CodePoint(0x31), expectingLine)
			assertParseFailureOnExpr(dut, CodePoint(0x31), expectingLine)
			for (inputChar <- characterCases) {
				it (s"throws when the next input is the character '${escape(inputChar)}'") {
					assertParseFailure(dut, (s"$inputChar" :: Nil, CodePoint(0)), List(expectingLine, s"\t$inputChar", "\t^"))
				}
			}
		}
		describe("Extractor.idExtractors.codePointWhere(_ => true)") {
			val expectingLine = "Expected '\\u0000'<=c<='\uDBFF\uDFFF'"
			val dut = Extractor.idExtractors.codePointWhere(_ => true)
			val characterCases = List[Int](0, 'a', 0xFFFF, CodePoint.MaxValue)

			assertParseFailureOnEmptyInput(dut, CodePoint(0x31), expectingLine)
			assertParseFailureOnExpr(dut, CodePoint(0x31), expectingLine)
			for (inputChar <- characterCases) {
				val scrutinee = 0
				val expecting = Some(Nil)
				it (s"returns ${expecting} when input is '${escape(inputChar)}' and scrutinee is '${escape(scrutinee)}'") {
					assertParseSuccess(dut, (s"${CodePoint(inputChar)}" :: Nil, CodePoint(scrutinee)), expecting)
				}
			}
			it ("matches the entire surrogate pair") {
				assertParseSuccess(dut, ("\uD83D\uDE00" :: Nil, CodePoint(128512)), Some(Nil))
			}
		}
		describe("Extractor.idExtractors.codePointWhere(c => 127137 <= c.intValue && c.intValue <= 127150)") {
			val expectingLine = "Expected '🂡'<=c<='🂮'"
			val dut = Extractor.idExtractors.codePointWhere(c => 127137 <= c.intValue && c.intValue <= 127150)
			val passCharCases = List[Int](127137, 127144, 127150)
			val failCharCases = List[Int](0, 127136, 127151, 0xFFFF, CodePoint.MaxValue)

			assertParseFailureOnEmptyInput(dut, CodePoint(0x31), expectingLine)
			assertParseFailureOnExpr(dut, CodePoint(0x31), expectingLine)
			for (inputChar <- passCharCases) {
				val scrutinee = 0
				val expecting = Some(Nil)
				it (s"returns ${expecting} when input is '${escape(inputChar)}' and scrutinee is '${escape(scrutinee)}'") {
					assertParseSuccess(dut, (s"${CodePoint(inputChar)}" :: Nil, CodePoint(scrutinee)), expecting)
				}
			}
			for (inputChar <- failCharCases) {
				it (s"throws when the next input is the character '${escape(inputChar)}'") {
					assertParseFailure(dut, (s"${CodePoint(inputChar)}" :: Nil, CodePoint(0)), List(expectingLine, s"\t${CodePoint(inputChar)}", "\t^"))
				}
			}
		}
	}
	describe("Extractor.idExtractors.end") {
		val expectingLine = "Expected EOF"
		val dut = Extractor.idExtractors.end

		it ("does match an empty input") {
			assertParseSuccess(dut, ("" :: Nil, ()), Option(Nil))
		}
		it ("throws when the next value is a character") {
			assertParseFailure(dut, ("value" :: Nil, ()), List(expectingLine, "\tvalue", "\t^"))
		}
		assertParseFailureOnExpr(dut, (), expectingLine)
	}
	describe("Extractor.idExtractors.ofType") {
		sealed trait Scrutinee
		object Scrutinee1 extends Scrutinee
		object Scrutinee2 extends Scrutinee

		val expectingLine = s"Expected OfType(${classOf[Scrutinee].getName})"
		val dut = Extractor.idExtractors.ofType[Scrutinee](classOf[Scrutinee])

		assertParseFailureOnEmptyInput(dut, Scrutinee2, expectingLine)
		it ("throws when the next value is a character") {
			assertParseFailure(dut, ("value" :: Nil, Scrutinee2), List(expectingLine, "\tvalue", "\t^"))
		}
		it ("Returns the scrutinee when the next input is an expr") {
			assertParseSuccess(dut, ("" :: "value" :: Nil, Scrutinee1), Option(List(Scrutinee1)))
		}
	}
	describe("Extractor#contramap") {
		describe("ofType[Foo].contramap(Bar => Foo)") {
			final case class Foo(x:Int)
			final case class Bar(x:Int)

			val dut = Extractor.idExtractors.ofType(classOf[Foo]).contramap[Bar](bar => new Foo(bar.x))

			it ("Returns the mapped Foo when the next input is an Expr and the scrutinee is a Bar") {
				assertParseSuccess(dut, ("" :: "" :: Nil, Bar(42)), Option(List(Foo(42))))
			}
		}
	}
	describe("Extractor#widenWith") {
		describe("ofType.widenWith(_.isInstanceOf)") {
			trait Superclass
			final case class Subclass1(x:Int) extends Superclass
			final case class Subclass2(x:Int) extends Superclass

			val dut = Extractor.idExtractors.ofType(classOf[Subclass1])
					.widenWith[Superclass](PartialExprFunction[Id, Superclass, Subclass1]({(x:Superclass) => x.isInstanceOf[Subclass1]}, {(x:Superclass) => x.asInstanceOf[Subclass1]}))

			it ("on match") {
				assertParseSuccess(dut, ("" :: "" :: Nil, Subclass1(42)), Option(List(Subclass1(42))))
			}
			it ("on not match") {
				assertParseSuccess(dut, ("" :: "" :: Nil, Subclass2(42)), Option.empty)
			}
		}
	}
	describe("Extractor#andThen") {
		describe("digit andThen alpha") {
			val digitExpectingLine = "Expected '0'<=c<='9'"
			val alphaExpectingLine = "Expected 'a'<=c<='z'"
			val digitParser = Extractor.idExtractors.charWhere(c => '0' <= c && c <= '9')
			val alphaParser = Extractor.idExtractors.charWhere(c => 'a' <= c && c <= 'z')

			val dut:Extractor[Id, Class, (Char, Char)] = digitParser andThen alphaParser

			assertParseFailureOnEmptyInput(dut, ('\u0000', '\u0000'), digitExpectingLine)
			it ("throws when expression has only one char; points at EOF") {
				assertParseFailure(dut, ("0" :: Nil, ('\u0000', '\u0000')), List(alphaExpectingLine, "\t0", "\t ^"))
			}
			it ("passes when expression has two matching chars") {
				assertParseSuccess(dut, ("0a" :: Nil, ('\u0000', '\u0000')), Option(List()))
			}
			it ("throws when expression has incorrect first char; points at first char") {
				assertParseFailure(dut, ("~~" :: Nil, ('\u0000', '\u0000')), List(digitExpectingLine, "\t~~", "\t^"))
			}
			it ("throws when expression has incorrect second char; points at second char") {
				assertParseFailure(dut, ("0~" :: Nil, ('\u0000', '\u0000')), List(alphaExpectingLine, "\t0~", "\t ^"))
			}
		}
	}
	describe("Extractor#orElse") {
		implicit def discriminatedContraEithered[A, B]:typeclass.ContraEithered[Id, A, B, scala.util.Either[A, B]] = {
			typeclass.ContraEithered[Id, A, B, scala.util.Either[A, B]](
				PartialExprFunction[Id, scala.util.Either[A, B], A](
					it => it.isLeft,
					it => it.left.get
				),
				PartialExprFunction[Id, scala.util.Either[A, B], B](
					it => it.isRight,
					it => it.right.get
				)
			)
		}

		describe("\"left\" orElse \"right\"") {
			val left = Extractor.idExtractors.isString("left")
			val right = Extractor.idExtractors.isString("right")

			val dut:Extractor[Id, Class, Either[Unit, Unit]] = left.orElse(right)(discriminatedContraEithered)

			it ("when string is left and scrutinee is left, then matches") {
				assertParseSuccess(dut, ("left" :: Nil, Left(())), Option(List.empty))
			}
			it ("when string is left and scrutinee is right, then not matches") {
				assertParseSuccess(dut, ("left" :: Nil, Right(())), Option.empty)
			}
			it ("when string is right and scrutinee is left, then not matches") {
				assertParseSuccess(dut, ("right" :: Nil, Left(())), Option.empty)
			}
			it ("when string is right and scrutinee is right, then matches") {
				assertParseSuccess(dut, ("right" :: Nil, Right(())), Option(List.empty))
			}
			it ("throws if string is neither") {
				assertParseFailure(dut, ("neither" :: Nil, Left(())), List("Expected \"left\" or \"right\"", "\tneither", "\t^"))
			}
		}
		describe("parts that may consume input") {
			def charIn(s:String) = Extractor.idExtractors.charIn(s).contramap({(_:Unit) => ' '})

			val abcd = charIn("a") andThen charIn("b") andThen charIn("c") andThen charIn("d")
			val abzy = charIn("a") andThen charIn("b") andThen charIn("z") andThen charIn("y")
			val dut:Extractor[Id, Class, Either[Unit, Unit]] = abcd.orElse(abzy)(discriminatedContraEithered)

			it ("even if left branch consumes input, then backtracking") {
				assertParseSuccess(dut, ("abzy" :: Nil, Right(())), Option(List()))
			}
		}
	}
	describe("Extractor#repeat") {
		import com.rayrobdod.stringContextParserCombinator.RepeatStrategy._

		describe("ofType*") {
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val dut:Extractor[Id, Class, List[String]] = one.repeat(strategy = Greedy)

			for (inputLength <- 0 to 2; scrutineeLength <- 0 to (inputLength + 1)) {
				val input = List.fill(1 + inputLength)("")
				val scrutinee = List("a", "b", "c", "d", "e").take(scrutineeLength)

				if (inputLength == scrutineeLength) {
					it (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then matches") {
						assertParseSuccess(dut, (input, scrutinee), Option(scrutinee))
					}
				} else {
					it (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then not matches") {
						assertParseSuccess(dut, (input, scrutinee), Option.empty)
					}
				}
			}

			it ("will backtrack") {
				val dut2:Extractor[Id, Class, (List[String], String)] = dut.andThen(one)
				val input = List.fill(5)("")
				val scrutinee = (List("a", "b", "c"), "d")
				val expecting = List("a", "b", "c", "d")

				assertParseSuccess(dut2, (input, scrutinee), Option(expecting))
			}
		}
		describe("ofType*+") {
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val dut:Extractor[Id, Class, List[String]] = one.repeat(strategy = Possessive)

			for (inputLength <- 0 to 2; scrutineeLength <- 0 to (inputLength + 1)) {
				val input = List.fill(1 + inputLength)("")
				val scrutinee = List("a", "b", "c", "d", "e").take(scrutineeLength)

				if (inputLength == scrutineeLength) {
					it (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then matches") {
						assertParseSuccess(dut, (input, scrutinee), Option(scrutinee))
					}
				} else {
					it (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then not matches") {
						assertParseSuccess(dut, (input, scrutinee), Option.empty)
					}
				}
			}

			it ("will not backtrack") {
				val dut2:Extractor[Id, Class, (List[String], String)] = dut.andThen(one)
				val input = List.fill(4)("")
				val scrutinee = (List("a", "b", "c"), "d")

				assertParseFailure(dut2, (input, scrutinee), List("Expected OfType(java.lang.String)", "\t${}${}${}", "\t         ^"))
			}
		}
		describe("ofType*?") {
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val dut:Extractor[Id, Class, List[String]] = one.repeat(strategy = Lazy)

			for (inputLength <- 0 to 2; scrutineeLength <- 0 to (inputLength + 1)) {
				val input = List.fill(1 + inputLength)("")
				val scrutinee = List("a", "b", "c", "d", "e").take(scrutineeLength)

				if (0 == scrutineeLength) {
					it (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then matches") {
						assertParseSuccess(dut, (input, scrutinee), Option(Nil))
					}
				} else {
					it (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then not matches") {
						assertParseSuccess(dut, (input, scrutinee), Option.empty)
					}
				}
			}

			it ("will backtrack") {
				val dut2:Extractor[Id, Class, List[String]] = dut.andThen(Extractor.idExtractors.end)
				val input = List.fill(4)("")
				val scrutinee = List("a", "b", "c")
				val expecting = List("a", "b", "c")

				assertParseSuccess(dut2, (input, scrutinee), Option(expecting))
			}
		}
		describe("ofType+") {
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val dut:Extractor[Id, Class, List[String]] = one.repeat(1, strategy = Greedy)

			it (s"when input has length 0 then throws") {
				assertParseFailure(dut, ("" :: Nil, List()), List("Expected OfType(java.lang.String)", "\t", "\t^"))
			}

			for (inputLength <- 1 to 2; scrutineeLength <- 0 to (inputLength + 1)) {
				val input = List.fill(1 + inputLength)("")
				val scrutinee = List("a", "b", "c", "d", "e").take(scrutineeLength)

				if (inputLength == scrutineeLength) {
					it (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then matches") {
						assertParseSuccess(dut, (input, scrutinee), Option(scrutinee))
					}
				} else {
					it (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then not matches") {
						assertParseSuccess(dut, (input, scrutinee), Option.empty)
					}
				}
			}
		}
		describe("ofType?") {
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val dut:Extractor[Id, Class, List[String]] = one.repeat(0, 1, strategy = Greedy)

			for (inputLength <- 0 to 2; scrutineeLength <- 0 to (inputLength + 1)) {
				val input = List.fill(1 + inputLength)("")
				val scrutinee = List("a", "b", "c", "d", "e").take(scrutineeLength)

				if (if (inputLength == 0) {scrutineeLength == 0} else {scrutineeLength == 1}) {
					it (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then matches") {
						assertParseSuccess(dut, (input, scrutinee), Option(scrutinee))
					}
				} else {
					it (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then not matches") {
						assertParseSuccess(dut, (input, scrutinee), Option.empty)
					}
				}
			}
		}
		describe("pass*") {
			val one = Extractor.idExtractors.pass
			val dut = one.repeat(strategy = Greedy)

			it ("does not hang indefinitely") {
				val input = "413" :: Nil

				assertParseSuccess(dut, (input, ()), Option(Nil))
			}
		}
		describe("(ofType*)*") {
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val two:Extractor[Id, Class, List[String]] = one.repeat(strategy = Greedy)
			val dut:Extractor[Id, Class, List[List[String]]] = two.repeat(strategy = Greedy)

			it ("does not hang indefinitely") {
				val input = "" :: "" :: Nil
				val scrutinee = List(List("a"))

				assertParseSuccess(dut, (input, scrutinee), Option.empty)
			}
		}
		describe("ofType* with delim `,`") {
			val delim = Extractor.idExtractors.isString(",")
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val dut:Extractor[Id, Class, List[String]] = one.repeat(delimiter = delim, strategy = Greedy)

			for (inputLength <- 0 to 1; scrutineeLength <- 0 to (inputLength + 1)) {
				val input = List.fill(inputLength + 1)("")
				val scrutinee = List("a", "b", "c", "d", "e").take(scrutineeLength)

				if (inputLength == scrutineeLength) {
					it (s"when input has length ${inputLength} and scrutinee has length ${scrutineeLength}, then matches") {
						assertParseSuccess(dut, (input, scrutinee), Option(scrutinee))
					}
				} else {
					it (s"when input has length ${inputLength} with scrutinee has length ${scrutineeLength}, then not matches") {
						assertParseSuccess(dut, (input, scrutinee), Option.empty)
					}
				}
			}
			{
				val inputLength = 5
				val inputWithDelim = "" :: "," :: "," :: "," :: "," :: "" :: Nil
				val inputSansDelim = "" :: ""  :: ""  :: ""  :: ""  :: "" :: Nil
				val letters = List("a", "b", "c", "d", "e", "f", "g", "h")

				it (s"when input has length ${inputLength} with delims and scrutinee has length ${inputLength}, then matches") {
					val scrutinee = letters.take(inputLength)
					assertParseSuccess(dut, (inputWithDelim, scrutinee), Option(scrutinee))
				}
				it (s"when input has length ${inputLength} sans delims, then pattern only matches one input") {
					val scrutinee = letters.take(1)
					assertParseSuccess(dut, (inputSansDelim, scrutinee), Option(scrutinee))
				}
			}
		}
		describe("ofType{2,} with delim `,`") {
			val delim = Extractor.idExtractors.isString(",")
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val dut:Extractor[Id, Class, List[String]] = one.repeat(2, delimiter = delim, strategy = Greedy)

			it (s"when input has length 1, throws and reports expecting the delimiter") {
				val input = "" :: "" :: Nil
				val scrutinee = "a" :: Nil
				assertParseFailure(dut, (input, scrutinee), List("Expected \",\"", "\t${}", "\t   ^"))
			}
		}
		describe("`(a ~ b ~ c)*`") {
			def charIn(s:String) = Extractor.idExtractors.charIn(s)

			val one:Extractor[Id, Class, (Char, Char)] = charIn("a") andThen charIn("b")
			val two:Extractor[Id, Class, ((Char, Char), Char)] = one andThen charIn("c")
			val dut:Extractor[Id, Class, List[((Char, Char), Char)]] = two.repeat(strategy = Greedy)
			val chartriple = (('\u0000', '\u0000'), '\u0000')

			it ("matches \"\"") {
					assertParseSuccess(dut, ("" :: Nil, Nil), Option(Nil))
			}
			it ("does not match \"a\"") {
					assertParseFailure(dut, ("a" :: Nil, Nil), List("Expected CharIn(\"b\")", "\ta", "\t ^"))
			}
			it ("does match \"abcde\"") {
					assertParseSuccess(dut, ("abcde" :: Nil, chartriple :: Nil), Option(Nil))
			}
			it ("does not match \"abca\"") {
					assertParseFailure(dut, ("abca" :: Nil, chartriple :: Nil), List("Expected CharIn(\"b\")", "\tabca", "\t    ^"))
			}
		}
	}
	describe("Extractor#optionally") {
		import com.rayrobdod.stringContextParserCombinator.RepeatStrategy._

		describe("ofType?") {
			val one = Extractor.idExtractors.ofType[String](classOf[String])
			val dut:Extractor[Id, Class, Option[String]] = one.optionally(strategy = Greedy)

			it (s"when input has length 0 and scrutinee is None, then matches") {
				assertParseSuccess(dut, ("" :: Nil, None), Option(Nil))
			}
			it (s"when input has length 1 and scrutinee is None, then not matches") {
				assertParseSuccess(dut, ("" :: "" :: Nil, None), None)
			}
			it (s"when input has length 0 and scrutinee is Some, then not matches") {
				assertParseSuccess(dut, ("" :: Nil, Some("Some")), None)
			}
			it (s"when input has length 1 and scrutinee is Some, then matches") {
				assertParseSuccess(dut, ("" :: "" :: Nil, Some("Some")), Option(List("Some")))
			}
		}
	}
}