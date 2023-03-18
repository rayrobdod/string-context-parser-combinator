package com.rayrobdod.stringContextParserCombinator
package internal

import org.scalatest.funspec.AnyFunSpec
import com.rayrobdod.stringContextParserCombinator.RepeatStrategy._
import TestUtilities._

final class RepeatTest extends AnyFunSpec {
	describe ("Repeat") {
		it ("`a*` matches ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				"",
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42)
			)
			val parser = childParser.repeat(strategy = Greedy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a*` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")
			val childExpecting = SingleExpecting("CharIn(\"a\")", 43)

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"a",
					SinglePartInput("", 43),
					SingleExpecting("CharIn(\"a\")", 42) ++ SingleExpecting("CharIn(\"a\")", 43)
				),
				List(
					Success1(
						"",
						SinglePartInput("a", 42),
						EmptyExpecting
					)
				)
			)
			val parser = childParser.repeat(strategy = Greedy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a*` matches `aaaa`") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"aaaa",
					SinglePartInput("", 46),
					RepeatedExpecting("CharIn(\"a\")", 42 to 46)
				),
				List(
					Success1(
						"aaa",
						SinglePartInput("a", 45),
						RepeatedExpecting("CharIn(\"a\")", 42 to 44)
					),
					Success1(
						"aa",
						SinglePartInput("aa", 44),
						RepeatedExpecting("CharIn(\"a\")", 42 to 43)
					),
					Success1(
						"a",
						SinglePartInput("aaa", 43),
						RepeatedExpecting("CharIn(\"a\")", 42 to 42)
					),
					Success1(
						"",
						SinglePartInput("aaaa", 42),
						EmptyExpecting
					)
				)
			)
			val parser = childParser.repeat(strategy = Greedy)
			assertResult(expected){parser.interpolate(initialInput)}
		}

		it ("`a*+` matches ``, does not backtrack") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				"",
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42)
			)
			val parser = childParser.repeat(strategy = Possessive)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a*+` matches `a`, does not backtrack") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")
			val childExpecting = SingleExpecting("CharIn(\"a\")", 43)

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"a",
					SinglePartInput("", 43),
					SingleExpecting("CharIn(\"a\")", 42) ++ SingleExpecting("CharIn(\"a\")", 43)
				),
				List(
				)
			)
			val parser = childParser.repeat(strategy = Possessive)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a*+` matches `aaaa`, does not backtrack") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"aaaa",
					SinglePartInput("", 46),
					RepeatedExpecting("CharIn(\"a\")", 42 to 46)
				),
				List(
				)
			)
			val parser = childParser.repeat(strategy = Possessive)
			assertResult(expected){parser.interpolate(initialInput)}
		}

		it ("`a*?` matches ``, reverse priority") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				"",
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42)
			)
			val parser = childParser.repeat(strategy = Lazy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a*?` matches `a`, reverse priority") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")
			val childExpecting = SingleExpecting("CharIn(\"a\")", 43)

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"",
					SinglePartInput("a", 42),
					EmptyExpecting
				),
				List(
					Success1(
						"a",
						SinglePartInput("", 43),
						SingleExpecting("CharIn(\"a\")", 42) ++ SingleExpecting("CharIn(\"a\")", 43)
					)
				)
			)
			val parser = childParser.repeat(strategy = Lazy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a*?` matches `aaaa`, reverse priorty") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"",
					SinglePartInput("aaaa", 42),
					EmptyExpecting
				),
				List(
					Success1(
						"a",
						SinglePartInput("aaa", 43),
						RepeatedExpecting("CharIn(\"a\")", 42 to 42)
					),
					Success1(
						"aa",
						SinglePartInput("aa", 44),
						RepeatedExpecting("CharIn(\"a\")", 42 to 43)
					),
					Success1(
						"aaa",
						SinglePartInput("a", 45),
						RepeatedExpecting("CharIn(\"a\")", 42 to 44)
					),
					Success1(
						"aaaa",
						SinglePartInput("", 46),
						RepeatedExpecting("CharIn(\"a\")", 42 to 46)
					)
				)
			)
			val parser = childParser.repeat(strategy = Lazy)
			assertResult(expected){parser.interpolate(initialInput)}
		}

		it ("`a+` does not match ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Failure(
				SingleExpecting("CharIn(\"a\")", 42)
			)
			val parser = childParser.repeat(1, strategy = Greedy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a+` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				"a",
				SinglePartInput("", 43),
				RepeatedExpecting("CharIn(\"a\")", 42 to 43)
			)
			val parser = childParser.repeat(1, strategy = Greedy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a+` matches `aaaa`") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"aaaa",
					SinglePartInput("", 46),
					RepeatedExpecting("CharIn(\"a\")", 42 to 46)
				),
				List(
					Success1(
						"aaa",
						SinglePartInput("a", 45),
						RepeatedExpecting("CharIn(\"a\")", 42 to 44)
					),
					Success1(
						"aa",
						SinglePartInput("aa", 44),
						RepeatedExpecting("CharIn(\"a\")", 42 to 43)
					),
					Success1(
						"a",
						SinglePartInput("aaa", 43),
						RepeatedExpecting("CharIn(\"a\")", 42 to 42)
					)
				)
			)
			val parser = childParser.repeat(1, strategy = Greedy)
			assertResult(expected){parser.interpolate(initialInput)}
		}

		it ("`a++` does not match ``, does not backtrack") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Failure(
				SingleExpecting("CharIn(\"a\")", 42)
			)
			val parser = childParser.repeat(1, strategy = Possessive)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a++` matches `a`, does not backtrack") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				"a",
				SinglePartInput("", 43),
				RepeatedExpecting("CharIn(\"a\")", 42 to 43)
			)
			val parser = childParser.repeat(1, strategy = Possessive)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a++` matches `aaaa`, does not backtrack") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"aaaa",
					SinglePartInput("", 46),
					RepeatedExpecting("CharIn(\"a\")", 42 to 46)
				),
				List(
				)
			)
			val parser = childParser.repeat(1, strategy = Possessive)
			assertResult(expected){parser.interpolate(initialInput)}
		}

		it ("`a+?` does not match ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Failure(
				SingleExpecting("CharIn(\"a\")", 42)
			)
			val parser = childParser.repeat(1, strategy = Lazy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a+?` matches `a`, reverse priorty") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				"a",
				SinglePartInput("", 43),
				RepeatedExpecting("CharIn(\"a\")", 42 to 43)
			)
			val parser = childParser.repeat(1, strategy = Lazy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a+?` matches `aaaa`, reverse priorty") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"a",
					SinglePartInput("aaa", 43),
					RepeatedExpecting("CharIn(\"a\")", 42 to 42)
				),
				List(
					Success1(
						"aa",
						SinglePartInput("aa", 44),
						RepeatedExpecting("CharIn(\"a\")", 42 to 43)
					),
					Success1(
						"aaa",
						SinglePartInput("a", 45),
						RepeatedExpecting("CharIn(\"a\")", 42 to 44)
					),
					Success1(
						"aaaa",
						SinglePartInput("", 46),
						RepeatedExpecting("CharIn(\"a\")", 42 to 46)
					)
				)
			)
			val parser = childParser.repeat(1, strategy = Lazy)
			assertResult(expected){parser.interpolate(initialInput)}
		}

		it ("`a?` matches ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				"",
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42)
			)
			val parser = childParser.repeat(0, 1, strategy = Greedy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a?` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"a",
					SinglePartInput("", 43),
					SingleExpecting("CharIn(\"a\")", 42)
				),
				List(
					Success1(
						"",
						SinglePartInput("a", 42),
						EmptyExpecting
					)
				)
			)
			val parser = childParser.repeat(0, 1, strategy = Greedy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a?` does not match all of `aaaa`") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"a",
					SinglePartInput("aaa", 43),
					SingleExpecting("CharIn(\"a\")", 42)
				),
				List(
					Success1(
						"",
						SinglePartInput("aaaa", 42),
						EmptyExpecting
					)
				)
			)
			val parser = childParser.repeat(0, 1, strategy = Greedy)
			assertResult(expected){parser.interpolate(initialInput)}
		}

		it ("`a?+` matches ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				"",
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42)
			)
			val parser = childParser.repeat(0, 1, strategy = Possessive)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a?+` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"a",
					SinglePartInput("", 43),
					SingleExpecting("CharIn(\"a\")", 42)
				),
				List(
				)
			)
			val parser = childParser.repeat(0, 1, strategy = Possessive)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a?+` does not match all of `aaaa`") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"a",
					SinglePartInput("aaa", 43),
					SingleExpecting("CharIn(\"a\")", 42)
				),
				List(
				)
			)
			val parser = childParser.repeat(0, 1, strategy = Possessive)
			assertResult(expected){parser.interpolate(initialInput)}
		}

		it ("`a??` matches ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				"",
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42)
			)
			val parser = childParser.repeat(0, 1, strategy = Lazy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a??` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"",
					SinglePartInput("a", 42),
					EmptyExpecting
				),
				List(
					Success1(
						"a",
						SinglePartInput("", 43),
						SingleExpecting("CharIn(\"a\")", 42)
					)
				)
			)
			val parser = childParser.repeat(0, 1, strategy = Lazy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a??` does not match all of `aaaa`") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"",
					SinglePartInput("aaaa", 42),
					EmptyExpecting
				),
				List(
					Success1(
						"a",
						SinglePartInput("aaa", 43),
						SingleExpecting("CharIn(\"a\")", 42)
					)
				)
			)
			val parser = childParser.repeat(0, 1, strategy = Lazy)
			assertResult(expected){parser.interpolate(initialInput)}
		}

		it ("`<pass>*` does not hang indefinitely") {
			val initialInput = SinglePartInput("", 42)
			val childParser = new Pass

			val expected = Success[Nothing, StubPosition, Unit](
				Success1(
					(),
					SinglePartInput("", 42),
					EmptyExpecting
				),
				List(
					Success1(
						(),
						SinglePartInput("", 42),
						EmptyExpecting
					)
				)
			)
			val parser = childParser.repeat(strategy = Greedy)

			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`(a*)*` does not hang indefinitely") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, Seq[String]](
				Success1(
					Seq(""),
					SinglePartInput("", 42),
					SingleExpecting("CharIn(\"a\")", 42)
				),
				List(
					Success1(
						Seq(),
						SinglePartInput("", 42),
						EmptyExpecting
					)
				)
			)
			val parser = childParser.repeat(strategy = Greedy).repeat(strategy = Greedy)

			assertResult(expected){parser.interpolate(initialInput)}
		}

		it ("`a*` with delim `b` matches ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")
			val delimParser = CharIn("b").map(_ => ())

			val expected = Success[Nothing, StubPosition, String](
				"",
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42)
			)
			val parser = childParser.repeat(delimiter = delimParser, strategy = Greedy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a*` with delim `b` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")
			val delimParser = CharIn("b").map(_ => ())

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"a",
					SinglePartInput("", 43),
					SingleExpecting("CharIn(\"a\")", 42) ++
						SingleExpecting("CharIn(\"b\")", 43)
				),
				List(
					Success1(
						"",
						SinglePartInput("a", 42),
						EmptyExpecting
					)
				)
			)
			val parser = childParser.repeat(delimiter = delimParser, strategy = Greedy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a*` with delim `b` matches `ababa`") {
			val initialInput = SinglePartInput("ababa", 42)
			val childParser = CharIn("a")
			val delimParser = CharIn("b").map(_ => ())

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"aaa",
					SinglePartInput("", 47),
					RepeatedExpecting("CharIn(\"a\")", 42 to 46 by 2) ++
						RepeatedExpecting("CharIn(\"b\")", 43 to 47 by 2)
				),
				List(
					Success1(
						"aa",
						SinglePartInput("ba", 45),
						RepeatedExpecting("CharIn(\"a\")", 42 to 44 by 2) ++
							RepeatedExpecting("CharIn(\"b\")", 43 to 43 by 2)
					),
					Success1(
						"a",
						SinglePartInput("baba", 43),
						SingleExpecting("CharIn(\"a\")", 42)
					),
					Success1(
						"",
						SinglePartInput("ababa", 42),
						EmptyExpecting
					)
				)
			)
			val parser = childParser.repeat(delimiter = delimParser, strategy = Greedy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a{2,}` with delim `b` does not match `a` and report expecting 'b'") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")
			val delimParser = CharIn("b").map(_ => ())

			val expected = Failure(
				SingleExpecting("CharIn(\"a\")", 42) ++
					SingleExpecting("CharIn(\"b\")", 43)
			)
			val parser = childParser.repeat(min = 2, delimiter = delimParser, strategy = Greedy)
			assertResult(expected){parser.interpolate(initialInput)}
		}

		describe("`(a ~ b ~ c)*`") {
			val childParser = (CharIn("a")
				andThen CharIn("b")
				andThen CharIn("c"))
			val parser = childParser.repeat(strategy = Greedy)

			it ("matches ``; no cut") {
				val initialInput = SinglePartInput("zzz", 42)

				val expected = Success[Nothing, StubPosition, List[Nothing]](
					List(),
					SinglePartInput("zzz", 42),
					SingleExpecting("CharIn(\"a\")", 42)
				)
				assertResult(expected){parser.interpolate(initialInput)}
			}
			it ("does not match `a`; has cut") {
				val initialInput = SinglePartInput("a", 42)

				val expected = Failure(
					SingleExpecting("CharIn(\"b\")", 43)
				)
				assertResult(expected){parser.interpolate(initialInput)}
			}
			it ("matches `abcde`; has cut") {
				val initialInput = SinglePartInput("abcde", 42)

				val expected = Success[Nothing, StubPosition, List[((Char, Char), Char)]](
					Success1(
						List((('a','b'),'c')),
						SinglePartInput("de", 45),
						SingleExpecting("CharIn(\"b\")", 43) ++
							SingleExpecting("CharIn(\"c\")", 44) ++
							SingleExpecting("CharIn(\"a\")", 45)
					),
					List(
						Success1(
							List(),
							SinglePartInput("abcde", 42),
							EmptyExpecting
						)
					)
				)
				assertResult(expected){parser.interpolate(initialInput)}
			}
			it ("does not match `abca`; has cut") {
				val initialInput = SinglePartInput("abca", 42)

				val expected = Failure(
					SingleExpecting("CharIn(\"b\")", 43) ++
						SingleExpecting("CharIn(\"c\")", 44) ++
						SingleExpecting("CharIn(\"b\")", 46)
				)
				assertResult(expected){parser.interpolate(initialInput)}
			}
		}
	}
}
