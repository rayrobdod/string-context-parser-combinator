package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec

final class RepeatTest extends AnyFunSpec {
	def InputPart(str:String, pos:Int) = ((str, PositionPoint(pos)))

	describe ("Repeat") {
		it ("`a*` matches ``") {
			val initialInput = new Input[Nothing](InputPart("", 42) :: Nil, Nil)
			val childParser = CharIn("a")

			val expected = Success[Nothing, String](
				"",
				new Input[Nothing](InputPart("", 42) :: Nil, Nil),
				EmptyTrace(initialInput)
			)
			val parser = childParser.repeat()
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a*` matches `a`") {
			val initialInput = new Input[Nothing](InputPart("a", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val childExpecting = Expecting("CharIn(\"a\")")

			val expected = Success[Nothing, String](
				"a",
				new Input[Nothing](InputPart("", 43) :: Nil, Nil),
				LeafTrace(childExpecting, new Input(List(InputPart("a", 42)), List()))
			)
			val parser = childParser.repeat()
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a*` matches `aaaa`") {
			val initialInput = new Input[Nothing](InputPart("aaaa", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val childExpecting = Expecting("CharIn(\"a\")")

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
				)
			)
			val parser = childParser.repeat()
			assertResult(expected){parser.parse(initialInput)}
		}

		it ("`a+` does not match ``") {
			val initialInput = new Input[Nothing](InputPart("", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val childExpecting = Expecting("CharIn(\"a\")")

			val expected = Failure[Nothing](
				LeafTrace(childExpecting, new Input(List(InputPart("", 42)), List()))
			)
			val parser = childParser.repeat(1)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a+` matches `a`") {
			val initialInput = new Input[Nothing](InputPart("a", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val childExpecting = Expecting("CharIn(\"a\")")

			val expected = Success[Nothing, String](
				"a",
				new Input[Nothing](InputPart("", 43) :: Nil, Nil),
				LeafTrace(childExpecting, new Input(List(InputPart("a", 42)), List()))
			)
			val parser = childParser.repeat(1)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a+` matches `aaaa`") {
			val initialInput = new Input[Nothing](InputPart("aaaa", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val childExpecting = Expecting("CharIn(\"a\")")

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
				)
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
				EmptyTrace(initialInput)
			)
			val parser = childParser.repeat(0, 1)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a?` matches `a`") {
			val initialInput = new Input[Nothing](InputPart("a", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val childExpecting = Expecting("CharIn(\"a\")")

			val expected = Success[Nothing, String](
				"a",
				new Input[Nothing](InputPart("", 43) :: Nil, Nil),
				LeafTrace(childExpecting, new Input(List(InputPart("a", 42)), List()))
			)
			val parser = childParser.repeat(0, 1)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a?` does not match all of `aaaa`") {
			val initialInput = new Input[Nothing](InputPart("aaaa", 42) :: Nil, Nil)
			val childParser = CharIn("a")
			val childExpecting = Expecting("CharIn(\"a\")")

			val expected = Success[Nothing, String](
				"a",
				new Input[Nothing](InputPart("aaa", 43) :: Nil, Nil),
				LeafTrace(childExpecting, new Input(List(InputPart("aaaa", 42)), List()))
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
				EmptyTrace(initialInput)
			)
			val parser = childParser.repeat().repeat()
			assertResult(expected){parser.parse(initialInput)}
		}
	}
}
