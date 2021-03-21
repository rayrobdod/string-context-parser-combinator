package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec

final class RepeatTest extends AnyFunSpec {
	def InputPart(str:String, pos:Int) = ((str, Position(pos)))

	describe ("Repeat") {
		it ("`a*` matches ``") {
			val initialInput = new Input[Nothing](InputPart("", 42) :: Nil, Nil)
			val childParser = CharIn("a")

			val expected = Success[Nothing, String](
				"",
				new Input[Nothing](InputPart("", 42) :: Nil, Nil),
				EmptyTrace(initialInput),
				Cut.False
			)
			val parser = childParser.repeat()
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a*` matches `a`") {
			val initialInput = new Input[Nothing](InputPart("a", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val childExpecting = ExpectingDescription("CharIn(\"a\")")

			val expected = Success[Nothing, String](
				"a",
				new Input[Nothing](InputPart("", 43) :: Nil, Nil),
				LeafTrace(childExpecting, new Input(List(InputPart("a", 42)), List())),
				Cut.False
			)
			val parser = childParser.repeat()
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a*` matches `aaaa`") {
			val initialInput = new Input[Nothing](InputPart("aaaa", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val childExpecting = ExpectingDescription("CharIn(\"a\")")

			val expected = Success[Nothing, String](
				"aaaa",
				new Input[Nothing](InputPart("", 46) :: Nil, Nil),
				ThenTrace(
					ThenTrace(
						ThenTrace(
							LeafTrace(childExpecting, new Input(List(InputPart("aaaa", 42)), List())),
							LeafTrace(childExpecting, new Input(List(InputPart("aaa", 43)), List()))
						),
						LeafTrace(childExpecting, new Input(List(InputPart("aa", 44)), List()))
					),
					LeafTrace(childExpecting, new Input(List(InputPart("a", 45)), List()))
				),
				Cut.False
			)
			val parser = childParser.repeat()
			assertResult(expected){parser.parse(initialInput)}
		}

		it ("`a+` does not match ``") {
			val initialInput = new Input[Nothing](InputPart("", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val childExpecting = ExpectingDescription("CharIn(\"a\")")

			val expected = Failure[Nothing](
				LeafTrace(childExpecting, new Input(List(InputPart("", 42)), List())),
				Cut.False
			)
			val parser = childParser.repeat(1)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a+` matches `a`") {
			val initialInput = new Input[Nothing](InputPart("a", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val childExpecting = ExpectingDescription("CharIn(\"a\")")

			val expected = Success[Nothing, String](
				"a",
				new Input[Nothing](InputPart("", 43) :: Nil, Nil),
				LeafTrace(childExpecting, new Input(List(InputPart("a", 42)), List())),
				Cut.False
			)
			val parser = childParser.repeat(1)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a+` matches `aaaa`") {
			val initialInput = new Input[Nothing](InputPart("aaaa", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val childExpecting = ExpectingDescription("CharIn(\"a\")")

			val expected = Success[Nothing, String](
				"aaaa",
				new Input[Nothing](InputPart("", 46) :: Nil, Nil),
				ThenTrace(
					ThenTrace(
						ThenTrace(
							LeafTrace(childExpecting, new Input(List(InputPart("aaaa", 42)), List())),
							LeafTrace(childExpecting, new Input(List(InputPart("aaa", 43)), List()))
						),
						LeafTrace(childExpecting, new Input(List(InputPart("aa", 44)), List()))
					),
					LeafTrace(childExpecting, new Input(List(InputPart("a", 45)), List()))
				),
				Cut.False
			)
			val parser = childParser.repeat(1)
			assertResult(expected){parser.parse(initialInput)}
		}

		it ("`a?` matches ``") {
			val initialInput = new Input[Nothing](InputPart("", 42) :: Nil, Nil)
			val childParser = CharIn("a")

			val expected = Success[Nothing, String](
				"",
				new Input[Nothing](InputPart("", 42) :: Nil, Nil),
				EmptyTrace(initialInput),
				Cut.False
			)
			val parser = childParser.repeat(0, 1)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a?` matches `a`") {
			val initialInput = new Input[Nothing](InputPart("a", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val childExpecting = ExpectingDescription("CharIn(\"a\")")

			val expected = Success[Nothing, String](
				"a",
				new Input[Nothing](InputPart("", 43) :: Nil, Nil),
				LeafTrace(childExpecting, new Input(List(InputPart("a", 42)), List())),
				Cut.False
			)
			val parser = childParser.repeat(0, 1)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a?` does not match all of `aaaa`") {
			val initialInput = new Input[Nothing](InputPart("aaaa", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val childExpecting = ExpectingDescription("CharIn(\"a\")")

			val expected = Success[Nothing, String](
				"a",
				new Input[Nothing](InputPart("aaa", 43) :: Nil, Nil),
				LeafTrace(childExpecting, new Input(List(InputPart("aaaa", 42)), List())),
				Cut.False
			)
			val parser = childParser.repeat(0, 1)
			assertResult(expected){parser.parse(initialInput)}
		}

		it ("`a**` does not hang indefinitely") {
			val initialInput = new Input[Nothing](InputPart("", 42) :: Nil, Nil)
			val childParser = CharIn("a")

			val expected = Success[Nothing, Seq[String]](
				Seq(""),
				new Input[Nothing](InputPart("", 42) :: Nil, Nil),
				EmptyTrace(initialInput),
				Cut.False
			)
			val parser = childParser.repeat().repeat()
			assertResult(expected){parser.parse(initialInput)}
		}

		it ("`a*` with delim `b` matches ``") {
			val initialInput = new Input[Nothing](InputPart("", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val delimParser = CharIn("b").map(_ => ())

			val expected = Success[Nothing, String](
				"",
				new Input[Nothing](InputPart("", 42) :: Nil, Nil),
				EmptyTrace(initialInput),
				Cut.False
			)
			val parser = childParser.repeat(delimiter = delimParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a*` with delim `b` matches `a`") {
			val initialInput = new Input[Nothing](InputPart("a", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val delimParser = CharIn("b").map(_ => ())
			val childExpecting = ExpectingDescription("CharIn(\"a\")")

			val expected = Success[Nothing, String](
				"a",
				new Input[Nothing](InputPart("", 43) :: Nil, Nil),
				LeafTrace(childExpecting, new Input(List(InputPart("a", 42)), List())),
				Cut.False
			)
			val parser = childParser.repeat(delimiter = delimParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a*` with delim `b` matches `ababa`") {
			val initialInput = new Input[Nothing](InputPart("ababa", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val delimParser = CharIn("b").map(_ => ())
			val childExpecting = ExpectingDescription("CharIn(\"a\")")
			val delimExpecting = ExpectingDescription("CharIn(\"b\")")

			val expected = Success[Nothing, String](
				"aaa",
				new Input[Nothing](InputPart("", 47) :: Nil, Nil),
				ThenTrace(
					ThenTrace(
						ThenTrace(
							ThenTrace(
								LeafTrace(childExpecting, new Input(List(InputPart("ababa", 42)), List())),
								LeafTrace(delimExpecting, new Input(List(InputPart("baba", 43)), List()))
							),
							LeafTrace(childExpecting, new Input(List(InputPart("aba", 44)), List()))
						),
						LeafTrace(delimExpecting, new Input(List(InputPart("ba", 45)), List()))
					),
					LeafTrace(childExpecting, new Input(List(InputPart("a", 46)), List()))
				),
				Cut.False
			)
			val parser = childParser.repeat(delimiter = delimParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a{2,}` with delim `b` does not match `a` and report expecting 'b'") {
			val initialInput = new Input[Nothing](InputPart("a", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val delimParser = CharIn("b").map(_ => ())

			val expected = Failure[Nothing](
				ThenTrace(
					LeafTrace(ExpectingDescription("CharIn(\"a\")"), new Input(List(InputPart("a", 42)), List())),
					LeafTrace(ExpectingDescription("CharIn(\"b\")"), new Input(List(InputPart("", 43)), List()))
				),
				Cut.False
			)
			val parser = childParser.repeat(min = 2, delimiter = delimParser)
			assertResult(expected){parser.parse(initialInput)}
		}

		describe("`(a ~/ b ~ c)*`") {
			val childExpecting = ExpectingDescription("childExpecting")
			val childParser = (CharIn[Nothing]("a")
				andThenWithCut CharIn[Nothing]("b")
				andThen CharIn[Nothing]("c")).opaque("childExpecting")
			val parser = childParser.repeat()

			it ("matches ``; no cut") {
				val initialInput = new Input[Nothing](InputPart("zzz", 42) :: Nil, Nil)

				val expected = Success[Nothing, List[Nothing]](
					List(),
					new Input[Nothing](InputPart("zzz", 42) :: Nil, Nil),
					EmptyTrace(initialInput),
					Cut.False
				)
				assertResult(expected){parser.parse(initialInput)}
			}
			it ("does not match `a`; has cut") {
				val initialInput = new Input[Nothing](InputPart("a", 42) :: Nil, Nil)

				val expected = Failure(
					LeafTrace(childExpecting, initialInput),
					Cut.True
				)
				assertResult(expected){parser.parse(initialInput)}
			}
			it ("matches `abcde`; has cut") {
				val initialInput = new Input[Nothing](InputPart("abcde", 42) :: Nil, Nil)

				val expected = Success[Nothing, List[((Char, Char), Char)]](
					List((('a','b'),'c')),
					new Input[Nothing](InputPart("de", 45) :: Nil, Nil),
					LeafTrace(childExpecting, initialInput),
					Cut.True
				)
				assertResult(expected){parser.parse(initialInput)}
			}
			it ("does not match `abca`; has cut") {
				val initialInput = new Input[Nothing](InputPart("abca", 42) :: Nil, Nil)

				val expected = Failure(
					ThenTrace(
						LeafTrace(childExpecting, initialInput),
						LeafTrace(childExpecting, new Input[Nothing](InputPart("a", 45) :: Nil, Nil))
					),
					Cut.True
				)
				assertResult(expected){parser.parse(initialInput)}
			}
		}
	}
}
