package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec

final class AndThen_Repeat_Test extends AnyFunSpec {
	def InputPart(str:String, pos:Int) = ((str, PositionPoint(pos)))
	def InputNoArgs(str:String, pos:Int) = new Input[Nothing](InputPart(str, pos) :: Nil, Nil)

	describe ("AndThen / Repeat") {
		it ("Zero-length input with any repeat fails and shows both inputs as options") {
			val initialInput = InputNoArgs("", 42)
			val leftParser = CharIn("a").repeat()
			val rightParser = CharIn("b")
			val leftExpecting = Expecting("CharIn(\"a\")")
			val rightExpecting = Expecting("CharIn(\"b\")")

			val expected = Failure(
				OrTrace(
					LeafTrace(leftExpecting, initialInput),
					LeafTrace(rightExpecting, initialInput)
				)
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("missing right with any repeat fails and shows both inputs as options") {
			val initialInput = InputNoArgs("a", 42)
			val leftParser = CharIn("a").repeat()
			val rightParser = CharIn("b")
			val leftExpecting = Expecting("CharIn(\"a\")")
			val rightExpecting = Expecting("CharIn(\"b\")")

			val expected = Failure(
				ThenTrace(
					LeafTrace(leftExpecting, initialInput),
					OrTrace(
						LeafTrace(leftExpecting, InputNoArgs("", 43)),
						LeafTrace(rightExpecting, InputNoArgs("", 43))
					)
				)
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("unexpected right with any repeat fails and shows both inputs as options") {
			val initialInput = InputNoArgs("ac", 42)
			val leftParser = CharIn("a").repeat()
			val rightParser = CharIn("b")
			val leftExpecting = Expecting("CharIn(\"a\")")
			val rightExpecting = Expecting("CharIn(\"b\")")

			val expected = Failure(
				ThenTrace(
					LeafTrace(leftExpecting, initialInput),
					OrTrace(
						LeafTrace(leftExpecting, InputNoArgs("c", 43)),
						LeafTrace(rightExpecting, InputNoArgs("c", 43))
					)
				)
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("input too short for repeat") {
			val initialInput = InputNoArgs("a", 42)
			val leftParser = CharIn("a").repeat(3,5)
			val rightParser = CharIn("b")
			val leftExpecting = Expecting("CharIn(\"a\")")

			val expected = Failure(
				ThenTrace(
					LeafTrace(leftExpecting, initialInput),
					LeafTrace(leftExpecting, InputNoArgs("", 43))
				)
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("input too long for repeat") {
			val initialInput = InputNoArgs("aaaaaaaa", 42)
			val leftParser = CharIn("a").repeat(3,5)
			val rightParser = CharIn("b")
			val leftExpecting = Expecting("CharIn(\"a\")")
			val rightExpecting = Expecting("CharIn(\"b\")")

			val expected = Failure(
				ThenTrace(
					ThenTrace(
						ThenTrace(
							ThenTrace(
								ThenTrace(
									LeafTrace(leftExpecting, initialInput),
									LeafTrace(leftExpecting, InputNoArgs("aaaaaaa", 43))
								),
								LeafTrace(leftExpecting, InputNoArgs("aaaaaa", 44))
							),
							LeafTrace(leftExpecting, InputNoArgs("aaaaa", 45))
						),
						LeafTrace(leftExpecting, InputNoArgs("aaaa", 46))
					),
					LeafTrace(rightExpecting, InputNoArgs("aaa", 47))
				)
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("pattern.repeat andThen subset") {
			val initialInput = InputNoArgs("aa", 42)
			val leftParser = CharIn("ab").repeat()
			val rightParser = CharIn("a")
			val leftExpecting = Expecting("CharIn(\"ab\")")
			val rightExpecting = Expecting("CharIn(\"a\")")

			val expected = Success(
				("a", 'a'),
				InputNoArgs("", 44),
				ThenTrace(
					LeafTrace(leftExpecting, InputNoArgs("aa", 42)),
					LeafTrace(rightExpecting, InputNoArgs("a", 43))
				)
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("pattern.repeat andThen subset twice") {
			val initialInput = InputNoArgs("aaa", 42)
			val leftParser = CharIn("ab").repeat()
			val rightParser = CharIn("a")
			val leftExpecting = Expecting("CharIn(\"ab\")")
			val rightExpecting = Expecting("CharIn(\"a\")")

			val expected = Success(
				("aa", 'a'),
				InputNoArgs("", 45),
				ThenTrace(
					ThenTrace(
						LeafTrace(leftExpecting, InputNoArgs("aaa", 42)),
						LeafTrace(leftExpecting, InputNoArgs("aa", 43))
					),
					LeafTrace(rightExpecting, InputNoArgs("a", 44))
				)
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
	}
}
