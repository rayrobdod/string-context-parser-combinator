package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec

final class RepeatTest extends AnyFunSpec {
	val positionOfNothing:Nothing => Position = x => x
	def SinglePartInput(str:String, pos:Int) = new Input[Nothing](((str, Position(pos))) :: Nil, Nil, positionOfNothing)
	def SingleExpecting(msg:String, pos:Int) = Set(Expecting(msg, Position(pos)))

	describe ("Repeat") {
		it ("`a*` matches ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, String](
				"",
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42),
				Cut.False
			)
			val parser = childParser.repeat()
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a*` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")
			val childExpecting = SingleExpecting("CharIn(\"a\")", 43)

			val expected = Success[Nothing, String](
				"a",
				SinglePartInput("", 43),
				childExpecting,
				Cut.False
			)
			val parser = childParser.repeat()
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a*` matches `aaaa`") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, String](
				"aaaa",
				SinglePartInput("", 46),
				SingleExpecting("CharIn(\"a\")", 46),
				Cut.False
			)
			val parser = childParser.repeat()
			assertResult(expected){parser.parse(initialInput)}
		}

		it ("`a+` does not match ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Failure(
				SingleExpecting("CharIn(\"a\")", 42),
				Cut.False
			)
			val parser = childParser.repeat(1)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a+` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, String](
				"a",
				SinglePartInput("", 43),
				SingleExpecting("CharIn(\"a\")", 43),
				Cut.False
			)
			val parser = childParser.repeat(1)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a+` matches `aaaa`") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, String](
				"aaaa",
				SinglePartInput("", 46),
				SingleExpecting("CharIn(\"a\")", 46),
				Cut.False
			)
			val parser = childParser.repeat(1)
			assertResult(expected){parser.parse(initialInput)}
		}

		it ("`a?` matches ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, String](
				"",
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42),
				Cut.False
			)
			val parser = childParser.repeat(0, 1)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a?` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, String](
				"a",
				SinglePartInput("", 43),
				Set.empty,
				Cut.False
			)
			val parser = childParser.repeat(0, 1)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a?` does not match all of `aaaa`") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, String](
				"a",
				SinglePartInput("aaa", 43),
				Set.empty,
				Cut.False
			)
			val parser = childParser.repeat(0, 1)
			assertResult(expected){parser.parse(initialInput)}
		}

		it ("`a**` does not hang indefinitely") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, Seq[String]](
				Seq(""),
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42),
				Cut.False
			)
			val parser = childParser.repeat().repeat()
			assertResult(expected){parser.parse(initialInput)}
		}

		it ("`a*` with delim `b` matches ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")
			val delimParser = CharIn("b").map(_ => ())

			val expected = Success[Nothing, String](
				"",
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42),
				Cut.False
			)
			val parser = childParser.repeat(delimiter = delimParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a*` with delim `b` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")
			val delimParser = CharIn("b").map(_ => ())

			val expected = Success[Nothing, String](
				"a",
				SinglePartInput("", 43),
				SingleExpecting("CharIn(\"b\")", 43),
				Cut.False
			)
			val parser = childParser.repeat(delimiter = delimParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a*` with delim `b` matches `ababa`") {
			val initialInput = SinglePartInput("ababa", 42)
			val childParser = CharIn("a")
			val delimParser = CharIn("b").map(_ => ())

			val expected = Success[Nothing, String](
				"aaa",
				SinglePartInput("", 47),
				SingleExpecting("CharIn(\"b\")", 47),
				Cut.False
			)
			val parser = childParser.repeat(delimiter = delimParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a{2,}` with delim `b` does not match `a` and report expecting 'b'") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")
			val delimParser = CharIn("b").map(_ => ())

			val expected = Failure(
				SingleExpecting("CharIn(\"b\")", 43),
				Cut.False
			)
			val parser = childParser.repeat(min = 2, delimiter = delimParser)
			assertResult(expected){parser.parse(initialInput)}
		}

		describe("`(a ~/ b ~ c)*`") {
			val childParser = (CharIn[Nothing]("a")
				andThenWithCut CharIn[Nothing]("b")
				andThen CharIn[Nothing]("c"))
			val parser = childParser.repeat()

			it ("matches ``; no cut") {
				val initialInput = SinglePartInput("zzz", 42)

				val expected = Success[Nothing, List[Nothing]](
					List(),
					SinglePartInput("zzz", 42),
					SingleExpecting("CharIn(\"a\")", 42),
					Cut.False
				)
				assertResult(expected){parser.parse(initialInput)}
			}
			it ("does not match `a`; has cut") {
				val initialInput = SinglePartInput("a", 42)

				val expected = Failure(
					SingleExpecting("CharIn(\"b\")", 43),
					Cut.True
				)
				assertResult(expected){parser.parse(initialInput)}
			}
			it ("matches `abcde`; has cut") {
				val initialInput = SinglePartInput("abcde", 42)

				val expected = Success[Nothing, List[((Char, Char), Char)]](
					List((('a','b'),'c')),
					SinglePartInput("de", 45),
					SingleExpecting("CharIn(\"a\")", 45),
					Cut.True
				)
				assertResult(expected){parser.parse(initialInput)}
			}
			it ("does not match `abca`; has cut") {
				val initialInput = SinglePartInput("abca", 42)

				val expected = Failure(
					SingleExpecting("CharIn(\"b\")", 46),
					Cut.True
				)
				assertResult(expected){parser.parse(initialInput)}
			}
		}
	}
}
