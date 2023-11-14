package name.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Seq
import munit.Location
import name.rayrobdod.stringContextParserCombinator.RepeatStrategy._

abstract class BaseInterpolatorSuite extends munit.FunSuite {
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
		case _ => CodePoint.unsafe_apply(in).toString
	}

	def assertParseSuccess[A](
		dut:Interpolator[Any, A],
		input:(List[String], Seq[Any]),
		expecting:A)(
		implicit loc: Location
	):Unit = {
		val result = dut.interpolate(StringContext(input._1:_*), input._2)
		assertEquals(result, expecting)
	}

	def assertParseFailure[A](
		dut:Interpolator[Any, A],
		input:(List[String], Seq[Any]),
		messageLines:List[String])(
		implicit loc: Location
	):Unit = {
		val expecting = messageLines.mkString("\n")
		val result = intercept[ParseException] {
			dut.interpolate(StringContext(input._1:_*), input._2)
		}
		assertEquals(result.getMessage(), expecting)
	}

	def assertParseFailureOnEmptyInput[A](
		dut:Interpolator[Any, A],
		expectingLine:String)(
		implicit loc: Location
	):Unit = {
		test ("throws when the input is empty") {
			assertParseFailure(dut, ("" :: Nil, Nil), List(expectingLine, "\t", "\t^"))
		}
	}

	def assertParseFailureOnExpr[A](
		dut:Interpolator[Any, A],
		expectingLine:String)(
		implicit loc: Location
	):Unit = {
		test ("throws when the next input is an expr") {
			assertParseFailure(dut, ("" :: "value" :: Nil, 23 :: Nil), List(expectingLine, "\t${}value", "\t^"))
		}
	}
}

package InterpolatorTest {
	package CharWhere {
		final class ConstFalse extends BaseInterpolatorSuite {
			val expectingLine = "Expected nothing"
			val dut = Interpolator.idInterpolators.charWhere(_ => false)
			val characterCases = List('\u0000', 'a', '\uFFFF', '\uD83D')

			assertParseFailureOnEmptyInput(dut, expectingLine)
			assertParseFailureOnExpr(dut, expectingLine)
			for (inputChar <- characterCases) {
				test (s"throws when the next input is the character '${escape(inputChar)}'") {
					assertParseFailure(dut, (s"$inputChar" :: Nil, Nil), List(expectingLine, s"\t$inputChar", "\t^"))
				}
			}
		}
		final class ConstTrue extends BaseInterpolatorSuite {
			val expectingLine = "Expected '\\u0000'<=c<='\uFFFF'"
			val dut = Interpolator.idInterpolators.charWhere(_ => true)
			val characterCases = List('\u0000', 'a', '\uFFFF')

			assertParseFailureOnEmptyInput(dut, expectingLine)
			assertParseFailureOnExpr(dut, expectingLine)
			for (inputChar <- characterCases) {
				test (s"returns the character when the input is a single-character string ('${escape(inputChar)}')") {
					assertParseSuccess(dut, (s"$inputChar" :: Nil, Nil), inputChar)
				}
			}
			test ("Returns the first character of a multiple-character input") {
				assertParseSuccess(dut, ("123" :: Nil, Nil), '1')
			}
		}
		final class IsAsciiDigit extends BaseInterpolatorSuite {
			val expectingLine = "Expected '0'<=c<='9'"
			val dut = Interpolator.idInterpolators.charWhere(c => '0' <= c && c <= '9')
			val passCharCases = List('0', '9')
			val failCharCases = List[Char]('a', '0' - 1, '9' + 1, '\u0000', '\uFFFF')

			assertParseFailureOnEmptyInput(dut, expectingLine)
			assertParseFailureOnExpr(dut, expectingLine)
			for (inputChar <- passCharCases) {
				val expecting = inputChar
				test (s"returns ${expecting} when input is '${escape(inputChar)}'") {
					assertParseSuccess(dut, (s"$inputChar" :: Nil, Nil), expecting)
				}
			}
			for (inputChar <- failCharCases) {
				test (s"throws when the next input is the character '${escape(inputChar)}'") {
					assertParseFailure(dut, (s"$inputChar" :: Nil, Nil), List(expectingLine, s"\t$inputChar", "\t^"))
				}
			}
		}
	}
	package CodePointWhere {
		final class ConstFalse extends BaseInterpolatorSuite {
			val expectingLine = "Expected nothing"
			val dut = Interpolator.idInterpolators.codePointWhere(_ => false)
			val characterCases = List('\u0000', 'a', '\uFFFF', '\uD83D')

			assertParseFailureOnEmptyInput(dut, expectingLine)
			assertParseFailureOnExpr(dut, expectingLine)
			for (inputChar <- characterCases) {
				test (s"throws when the next input is the character '${escape(inputChar)}'") {
					assertParseFailure(dut, (s"$inputChar" :: Nil, Nil), List(expectingLine, s"\t$inputChar", "\t^"))
				}
			}
		}
		final class ConstTrue extends BaseInterpolatorSuite {
			val expectingLine = "Expected '\\u0000'<=c<='\uDBFF\uDFFF'"
			val dut = Interpolator.idInterpolators.codePointWhere(_ => true)
			val characterCases = List[Int](0, 'a', 0xFFFF, CodePoint.MaxValue.intValue)

			assertParseFailureOnEmptyInput(dut, expectingLine)
			assertParseFailureOnExpr(dut, expectingLine)
			for (inputChar <- characterCases) {
				val expecting = CodePoint.unsafe_apply(inputChar)
				test (s"returns ${expecting} when input is '${escape(inputChar)}'") {
					assertParseSuccess(dut, (s"${CodePoint.unsafe_apply(inputChar)}" :: Nil, Nil), expecting)
				}
			}
			test ("matches the entire surrogate pair") {
				assertParseSuccess(dut, ("\uD83D\uDE00" :: Nil, Nil), CodePoint.unsafe_apply(128512))
			}
		}
		final class IsPlayingCard extends BaseInterpolatorSuite {
			val expectingLine = "Expected '🂡'<=c<='🂮'"
			val dut = Interpolator.idInterpolators.codePointWhere(c => 127137 <= c.intValue && c.intValue <= 127150)
			val passCharCases = List[Int](127137, 127144, 127150)
			val failCharCases = List[Int](0, 127136, 127151, 0xFFFF, CodePoint.MaxValue.intValue)

			assertParseFailureOnEmptyInput(dut, expectingLine)
			assertParseFailureOnExpr(dut, expectingLine)
			for (inputChar <- passCharCases) {
				val expecting = CodePoint.unsafe_apply(inputChar)
				test (s"returns ${expecting} when input is '${escape(inputChar)}'") {
					assertParseSuccess(dut, (s"${CodePoint.unsafe_apply(inputChar)}" :: Nil, Nil), expecting)
				}
			}
			for (inputChar <- failCharCases) {
				test (s"throws when the next input is the character '${escape(inputChar)}'") {
					assertParseFailure(dut, (s"${CodePoint.unsafe_apply(inputChar)}" :: Nil, Nil), List(expectingLine, s"\t${CodePoint.unsafe_apply(inputChar)}", "\t^"))
				}
			}
		}
	}
	final class End extends BaseInterpolatorSuite {
		val expectingLine = "Expected EOF"
		val dut = Interpolator.idInterpolators.end

		test ("returns unit when the input is empty") {
			assertParseSuccess(dut, ("" :: Nil, Nil), ())
		}
		test ("throws when the next value is a character") {
			assertParseFailure(dut, ("value" :: Nil, Nil), List(expectingLine, "\tvalue", "\t^"))
		}
		assertParseFailureOnExpr(dut, expectingLine)
	}
	final class OfType extends BaseInterpolatorSuite {
		sealed trait Scrutinee
		object Scrutinee1 extends Scrutinee
		object Scrutinee2 extends Scrutinee

		val expectingLine = s"Expected OfType(${classOf[Scrutinee].getName})"
		val dut = Interpolator.idInterpolators.ofType[Scrutinee]

		assertParseFailureOnEmptyInput(dut, expectingLine)
		test ("throws when the next value is a character") {
			assertParseFailure(dut, ("value" :: Nil, Nil), List(expectingLine, "\tvalue", "\t^"))
		}
		test ("Returns the next input when the next input is an expr") {
			assertParseSuccess(dut, ("" :: "value" :: Nil, Scrutinee1 :: Nil), Scrutinee1)
		}
	}
	final class Pass extends BaseInterpolatorSuite {
		val dut = Interpolator.idInterpolators.pass

		test ("returns unit when the input is empty") {
			assertParseSuccess(dut, ("" :: Nil, Nil), ())
		}
		test ("Returns unit when the next input is a char") {
			assertParseSuccess(dut, ("value" :: Nil, Nil), ())
		}
		test ("Returns unit when the next input is an expr") {
			assertParseSuccess(dut, ("" :: "value" :: Nil, 42 :: Nil), ())
		}
	}
	package map {
		final case class Foo(x:Int)
		final case class Bar(x:Int)

		final class BarToFoo extends BaseInterpolatorSuite {
			val dut = Interpolator.idInterpolators.ofType[Bar].map[Foo](bar => new Foo(bar.x))

			test ("Returns the mapped Foo when the next input is an a Expr Bar") {
				assertParseSuccess(dut, ("" :: "" :: Nil, Bar(42) :: Nil), Foo(42))
			}
		}
	}
	package void {
		final case class Bar(x:Int)

		final class VoidedBar extends BaseInterpolatorSuite {
			val dut = Interpolator.idInterpolators.ofType[Bar].void

			test ("When base matches an input, then dut result is unit") {
				assertParseSuccess(dut, ("" :: "" :: Nil, Bar(42) :: Nil), ())
			}
		}
	}
	package filter {
		final case class Foo(x:Int)

		final class IsEven extends BaseInterpolatorSuite {
			val dut = Interpolator.idInterpolators.ofType[Foo].filter(_.x % 2 == 0, "is even")
			val className = "name.rayrobdod.stringContextParserCombinator.InterpolatorTest.filter.Foo"

			test ("if base parser fails, parser passes through the failure") {
				assertParseFailure(dut, ("" :: Nil, Nil), List(s"Expected OfType($className)", "\t", "\t^"))
			}
			test ("if base parser succeeds and predicate succeeds for all branches, parser passes through the success") {
				assertParseSuccess(dut, ("" :: "" :: Nil, Foo(42) :: Nil), Foo(42))
			}
			test ("if base parser succeeds and predicate fails for all branches, returns a failure including the filter description") {
				assertParseFailure(dut, ("" :: "" :: Nil, Foo(11) :: Nil), List(s"Expected OfType($className) where is even", "\t${}", "\t^"))
			}
			test ("if base parser succeeds and predicate fails for some branches, returns a success with the variants that passed the predicate".ignore) {
				val left = Interpolator.idInterpolators.charIn("a").map(_ => "1").attempt
				val right = Interpolator.idInterpolators.charIn("a").map(_ => "2").attempt
				val either = (left orElse right)
				val dut = either.filter(_ == "2", "is two")

				val input = ("a" :: Nil, Nil)

				assertParseSuccess(either, input, "1")
				assertParseSuccess(dut, input, "2")
			}
		}
	}
	package andThen {
		final class DigitAndThenAlpha extends BaseInterpolatorSuite {
			val digitExpectingLine = "Expected '0'<=c<='9'"
			val alphaExpectingLine = "Expected 'a'<=c<='z'"
			val digitParser = Interpolator.idInterpolators.charWhere(c => '0' <= c && c <= '9')
			val alphaParser = Interpolator.idInterpolators.charWhere(c => 'a' <= c && c <= 'z')

			val dut:Interpolator[Any, (Char, Char)] = digitParser andThen alphaParser

			assertParseFailureOnEmptyInput(dut, digitExpectingLine)
			test ("throws when expression has only one char; points at EOF") {
				assertParseFailure(dut, ("0" :: Nil, Nil), List(alphaExpectingLine, "\t0", "\t ^"))
			}
			test ("passes when expression has two matching chars") {
				assertParseSuccess(dut, ("0a" :: Nil, Nil), ('0', 'a'))
			}
			test ("throws when expression has incorrect first char; points at first char") {
				assertParseFailure(dut, ("~~" :: Nil, Nil), List(digitExpectingLine, "\t~~", "\t^"))
			}
			test ("throws when expression has incorrect second char; points at second char") {
				assertParseFailure(dut, ("0~" :: Nil, Nil), List(alphaExpectingLine, "\t0~", "\t ^"))
			}
		}
	}
	final class orElse extends BaseInterpolatorSuite {
		import Interpolator.idInterpolators.charIn
		val leftParser = charIn("a") andThen charIn("b")
		val rightParser = charIn("x") andThen charIn("y")
		val dut = leftParser orElse rightParser

		test ("`Success | Whatever` returns that success") {
			assertParseSuccess(dut, ("ab" :: Nil, Nil), ('a', 'b'))
		}
		test ("`Advanced | Whatever` returns that failure") {
			assertParseFailure(dut, ("a1" :: Nil, Nil), List("Expected CharIn(\"b\")", "\ta1", "\t ^"))
		}
		test ("`NonConsume | Success` returns that success") {
			assertParseSuccess(dut, ("xy" :: Nil, Nil), ('x', 'y'))
		}
		test ("`NonConsume | Advanced` returns a failure that mentions only the cut branch") {
			assertParseFailure(dut, ("x1" :: Nil, Nil), List("Expected CharIn(\"y\")", "\tx1", "\t ^"))
		}
		test ("`NonConsume | NonConsume` returns a failure that mentions both branches") {
			assertParseFailure(dut, ("11" :: Nil, Nil), List("Expected CharIn(\"a\") or CharIn(\"x\")", "\t11", "\t^"))
		}
	}
	package repeat {
		import name.rayrobdod.stringContextParserCombinator.RepeatStrategy._
		import Interpolator.idInterpolators.{charIn, end}

		final class ZeroOrMoreGreedy extends BaseInterpolatorSuite {
			val simple = charIn("a").repeat(strategy = Greedy)
			val complex = (charIn("a").andThen(charIn("b")).andThen(charIn("c")).map(_ => "abc")).repeat(strategy = Greedy).orElse(charIn("a").map(_ => "a"))

			test ("matches empty input") {
				assertParseSuccess(simple, ("" :: Nil, Nil), "")
			}
			test ("matches single input") {
				assertParseSuccess(simple, ("a" :: Nil, Nil), "a")
			}
			test ("matches multiple input") {
				assertParseSuccess(simple, ("aaaa" :: Nil, Nil), "aaaa")
			}
			test ("matches single input (with backtracking)") {
				assertParseSuccess(simple.andThen(charIn("a")), ("a" :: Nil, Nil), ("", 'a'))
			}
			test ("matches multiple input (with backtracking)") {
				assertParseSuccess(simple.andThen(charIn("a")), ("aaaa" :: Nil, Nil), ("aaa", 'a'))
			}
			test ("matches empty input (with andThen)") {
				assertParseSuccess(complex, ("" :: Nil, Nil), Nil)
			}
			test ("does not match partial child and has cut") {
				assertParseFailure(complex, ("a" :: Nil, Nil), List("Expected CharIn(\"b\")", "\ta", "\t ^"))
			}
			test ("does not match full child followed by unrelated") {
				assertParseSuccess(complex, ("abcde" :: Nil, Nil), List("abc"))
			}
			test ("does not match full child followed by partial child and has cut") {
				assertParseFailure(complex, ("abca" :: Nil, Nil), List("Expected CharIn(\"b\")", "\tabca", "\t    ^"))
			}
		}
		final class ZeroOrMorePosessive extends BaseInterpolatorSuite {
			val simple = charIn("a").repeat(strategy = Possessive)

			test ("matches empty input") {
				assertParseSuccess(simple, ("" :: Nil, Nil), "")
			}
			test ("matches single input") {
				assertParseSuccess(simple, ("a" :: Nil, Nil), "a")
			}
			test ("matches multiple input") {
				assertParseSuccess(simple, ("aaaa" :: Nil, Nil), "aaaa")
			}
			test ("does not match single input with backtracking") {
				assertParseFailure(simple.andThen(charIn("a")), ("a" :: Nil, Nil), List("Expected CharIn(\"a\")", "\ta", "\t ^"))
			}
			test ("does not match multiple input with backtracking") {
				assertParseFailure(simple.andThen(charIn("a")), ("aaaa" :: Nil, Nil), List("Expected CharIn(\"a\")", "\taaaa", "\t    ^"))
			}
		}
		final class ZeroOrMoreLazy extends BaseInterpolatorSuite {
			val simple = charIn("a").repeat(strategy = Lazy)

			test ("matches empty input") {
				assertParseSuccess(simple, ("" :: Nil, Nil), "")
			}
			test ("matches single input") {
				assertParseSuccess(simple, ("a" :: Nil, Nil), "")
			}
			test ("matches multiple input") {
				assertParseSuccess(simple, ("aaaa" :: Nil, Nil), "")
			}
			test ("matches single input with backtracking") {
				assertParseSuccess(simple.andThen(end), ("a" :: Nil, Nil), "a")
			}
			test ("matches multiple input with backtracking") {
				assertParseSuccess(simple.andThen(end), ("aaaa" :: Nil, Nil), "aaaa")
			}
		}

		final class OneOrMoreGreedy extends BaseInterpolatorSuite {
			val simple = charIn("a").repeat(strategy = Greedy, min = 1)

			test ("does not match empty input") {
				assertParseFailure(simple, ("" :: Nil, Nil), List("Expected CharIn(\"a\")", "\t", "\t^"))
			}
			test ("matches single input") {
				assertParseSuccess(simple, ("a" :: Nil, Nil), "a")
			}
			test ("matches multiple input") {
				assertParseSuccess(simple, ("aaaa" :: Nil, Nil), "aaaa")
			}
		}
		final class OneOrMorePosessive extends BaseInterpolatorSuite {
			val simple = charIn("a").repeat(strategy = Possessive, min = 1)

			test ("does not match empty input") {
				assertParseFailure(simple, ("" :: Nil, Nil), List("Expected CharIn(\"a\")", "\t", "\t^"))
			}
			test ("matches single input") {
				assertParseSuccess(simple, ("a" :: Nil, Nil), "a")
			}
			test ("matches multiple input") {
				assertParseSuccess(simple, ("aaaa" :: Nil, Nil), "aaaa")
			}
		}
		final class OneOrMoreLazy extends BaseInterpolatorSuite {
			val simple = charIn("a").repeat(strategy = Lazy, min = 1)

			test ("does not match empty input") {
				assertParseFailure(simple, ("" :: Nil, Nil), List("Expected CharIn(\"a\")", "\t", "\t^"))
			}
			test ("matches single input") {
				assertParseSuccess(simple, ("a" :: Nil, Nil), "a")
			}
			test ("matches multiple input") {
				assertParseSuccess(simple, ("aaaa" :: Nil, Nil), "a")
			}
		}

		final class ZeroOrOneGreedy extends BaseInterpolatorSuite {
			val simple = charIn("a").repeat(strategy = Greedy, min = 0, max = 1)

			test ("does not match empty input") {
				assertParseSuccess(simple, ("" :: Nil, Nil), "")
			}
			test ("matches single input") {
				assertParseSuccess(simple, ("a" :: Nil, Nil), "a")
			}
			test ("matches multiple input") {
				assertParseSuccess(simple, ("aaaa" :: Nil, Nil), "a")
			}
		}
		final class ZeroOrOnePosessive extends BaseInterpolatorSuite {
			val simple = charIn("a").repeat(strategy = Possessive, min = 0, max = 1)

			test ("does not match empty input") {
				assertParseSuccess(simple, ("" :: Nil, Nil), "")
			}
			test ("matches single input") {
				assertParseSuccess(simple, ("a" :: Nil, Nil), "a")
			}
			test ("matches multiple input") {
				assertParseSuccess(simple, ("aaaa" :: Nil, Nil), "a")
			}
		}
		final class ZeroOrOneLazy extends BaseInterpolatorSuite {
			val simple = charIn("a").repeat(strategy = Lazy, min = 0, max = 1)

			test ("does not match empty input") {
				assertParseSuccess(simple, ("" :: Nil, Nil), "")
			}
			test ("matches single input") {
				assertParseSuccess(simple, ("a" :: Nil, Nil), "")
			}
			test ("matches multiple input") {
				assertParseSuccess(simple, ("aaaa" :: Nil, Nil), "")
			}
		}

		final class EmptyChildParserDoesNotCauseHang extends BaseInterpolatorSuite {
			test ("`<pass>*` does not hang indefinitely") {
				val dut = Interpolator.idInterpolators.pass.repeat(strategy = Greedy)
				assertParseSuccess(dut, ("" :: Nil, Nil), ())
			}
			test ("`(a*)*` does not hang indefinitely") {
				val dut = Interpolator.idInterpolators.charIn("a").repeat(strategy = Greedy).repeat(strategy = Greedy)
				assertParseSuccess(dut, ("aaaa" :: Nil, Nil), "aaaa" :: "" :: Nil)
			}
		}

		final class WithDelimiter extends BaseInterpolatorSuite {
			val simple = charIn("a").repeat(strategy = Greedy, delimiter = charIn("b").map(_ => ()))

			test ("matches empty input") {
				assertParseSuccess(simple, ("" :: Nil, Nil), "")
			}
			test ("matches single input") {
				assertParseSuccess(simple, ("a" :: Nil, Nil), "a")
			}
			test ("matches multiple input") {
				assertParseSuccess(simple, ("abababa" :: Nil, Nil), "aaaa")
			}

			test ("`a{2,}` with delim `b` does not match `a` and report expecting 'b'") {
				val simple = charIn("a").repeat(min = 2, strategy = Greedy, delimiter = charIn("b").map(_ => ()))
				assertParseFailure(simple, ("a" :: Nil, Nil), List("Expected CharIn(\"b\")", "\ta", "\t ^"))
			}
		}
	}
	package optionally {
		final class GreedyTest extends BaseInterpolatorSuite {
			val one = Interpolator.idInterpolators.ofType[String]
			val dut:Interpolator[Any, Option[String]] = one.optionally(strategy = Greedy)

			test ("when input is empty, then returns None") {
				assertParseSuccess(dut, ("" :: Nil, Nil), None)
			}
			test ("when input has length 1, then returns some with that value") {
				assertParseSuccess(dut, ("" :: "" :: Nil, "111" :: Nil), Some("111"))
			}
			test ("when input has length two-or-greater, then returns some with first value") {
				assertParseSuccess(dut, ("" :: "" :: "" :: "" :: Nil, "111" :: "222" :: "333" :: Nil), Some("111"))
			}
		}
		final class PossessiveTest extends BaseInterpolatorSuite {
			val one = Interpolator.idInterpolators.ofType[String]
			val dut:Interpolator[Any, Option[String]] = one.optionally(strategy = Possessive)

			test ("when input is empty, then returns None") {
				assertParseSuccess(dut, ("" :: Nil, Nil), None)
			}
			test ("when input has length 1, then returns some with that value") {
				assertParseSuccess(dut, ("" :: "" :: Nil, "111" :: Nil), Some("111"))
			}
			test ("when input has length two-or-greater, then returns some with first value") {
				assertParseSuccess(dut, ("" :: "" :: "" :: "" :: Nil, "111" :: "222" :: "333" :: Nil), Some("111"))
			}
		}
		final class LazyTest extends BaseInterpolatorSuite {
			val one = Interpolator.idInterpolators.ofType[String]
			val dut:Interpolator[Any, Option[String]] = one.optionally(strategy = Lazy)

			test ("when input is empty, then returns None") {
				assertParseSuccess(dut, ("" :: Nil, Nil), None)
			}
			test ("when input has length 1, then returns None") {
				assertParseSuccess(dut, ("" :: "" :: Nil, "111" :: Nil), None)
			}
			test ("when input has length two-or-greater, then returns None") {
				assertParseSuccess(dut, ("" :: "" :: "" :: "" :: Nil, "111" :: "222" :: "333" :: Nil), None)
			}
			test ("when input has length 1, will backtrack to return Some") {
				val dut2 = dut andThen Interpolator.end
				assertParseSuccess(dut2, ("" :: "" :: Nil, "111" :: Nil), Some("111"))
			}
		}
	}

	final class ComboAndThenRepeat extends BaseInterpolatorSuite {
		import Interpolator.idInterpolators.charIn
		import Interpolator.idInterpolators.isString

		test ("If the repeat is permissibly empty and right fails, then fails and shows both inputs as expected options") {
			val dut = isString("left").repeat(strategy = Greedy).andThen(isString("right"))
			assertParseFailure(dut, ("" :: Nil, Nil), List("Expected \"left\" or \"right\"", s"\t", "\t^"))
		}
		test ("If the repeat is permissibly empty and right succeeds, then result is a success") {
			val dut = isString("left").repeat(strategy = Greedy).andThen(isString("right"))
			assertParseSuccess(dut, ("right" :: Nil, Nil), ())
		}
		test ("If the repeat has consumed input, then the result matches that failure") {
			val dut = (isString("le").andThen(isString("ft"))).repeat(strategy = Greedy).andThen(isString("right"))
			assertParseFailure(dut, ("le" :: Nil, Nil), List("Expected \"ft\"", s"\tle", "\t  ^"))
		}
		test ("missing right with any repeat fails and shows both inputs as options") {
			val dut = isString("left").repeat(strategy = Greedy).andThen(isString("right"))
			assertParseFailure(dut, ("left" :: Nil, Nil), List("Expected \"left\" or \"right\"", s"\tleft", "\t    ^"))
		}
		test ("unexpected right with any repeat fails and shows both inputs as options") {
			val dut = isString("a").repeat(strategy = Greedy).andThen(isString("b"))
			assertParseFailure(dut, ("ac" :: Nil, Nil), List("Expected \"a\" or \"b\"", s"\tac", "\t ^"))
		}
		test ("input too short for repeat") {
			val dut = isString("a").repeat(3,5, strategy = Greedy).andThen(isString("b"))
			assertParseFailure(dut, ("a" :: Nil, Nil), List("Expected \"a\"", s"\ta", "\t ^"))
		}
		test ("input too long for repeat") {
			val dut = isString("a").repeat(3,5, strategy = Greedy).andThen(isString("b"))
			assertParseFailure(dut, ("aaaaaaaa" :: Nil, Nil), List("Expected \"b\"", s"\taaaaaaaa", "\t     ^"))
		}
		test ("pattern.repeat(Greedy) andThen subset") {
			val dut = charIn("ab").repeat(strategy = Greedy).andThen(charIn("a"))
			assertParseSuccess(dut, ("aa" :: Nil, Nil), ("a", 'a'))
		}
		test ("pattern.repeat(Greedy) andThen subset twice") {
			val dut = charIn("ab").repeat(strategy = Greedy).andThen(charIn("a"))
			assertParseSuccess(dut, ("aaa" :: Nil, Nil), ("aa", 'a'))
		}
		test ("pattern.repeat(Possessive) andThen subset") {
			val dut = charIn("ab").repeat(strategy = Possessive).andThen(charIn("a"))
			assertParseFailure(dut, ("aa" :: Nil, Nil), List("Expected CharIn(\"a\") or CharIn(\"ab\")", s"\taa", "\t  ^"))
		}
		test ("unexpected right with any repeat and delimiter fails and shows delimiter and right as options") {
			val dut = isString("a").repeat(delimiter = isString("b"), strategy = Greedy).andThen(isString("c"))
			assertParseFailure(dut, ("az" :: Nil, Nil), List("Expected \"b\" or \"c\"", s"\taz", "\t ^"))
		}
		test ("successful with delimiter") {
			val dut = isString("a").repeat(delimiter = isString("b"), strategy = Greedy).andThen(isString("c"))
			assertParseSuccess(dut, ("abac" :: Nil, Nil), ())
		}

		test ("right associative variant of \"'[' ~ 'a'.rep(',') ~ ']' reports the delimiter as an option when the suffix is not found\"") {
			val dut = isString("[") andThen (isString("a").repeat(delimiter = isString(",")) andThen isString("]"))
			assertParseFailure(dut, ("[a,a" :: Nil, Nil), List("Expected \",\" or \"]\"", s"\t[a,a", "\t    ^"))
		}
		test ("'[' ~ 'a'.rep(',') ~ ']' reports the delimiter as an option when the suffix is not found") {
			val dut = isString("[") andThen isString("a").repeat(delimiter = isString(",")) andThen isString("]")
			assertParseFailure(dut, ("[a,a" :: Nil, Nil), List("Expected \",\" or \"]\"", s"\t[a,a", "\t    ^"))
		}
	}
}
