package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec

final class AndThen_Repeat_Test extends AnyFunSpec {
	final case class SuccessValue(x:Int)
	def InputPart(str:String, pos:Int) = ((str, Position(pos)))
	def InputNoArgs(str:String, pos:Int) = new Input[Nothing](InputPart(str, pos) :: Nil, Nil)

	describe ("AndThen / Repeat") {
		val initialInput = InputNoArgs("initial", 1234)

		val cases:Seq[(String, (Int, Int), Seq[Sequence.Output[SuccessValue]], Parser[Nothing, SuccessValue], Result[Nothing, (Seq[SuccessValue], SuccessValue)])] = Seq(
			(
				"If the repeat is permissibly empty, and right fails, then fails and shows both inputs as expected options",
				(0, Integer.MAX_VALUE),
				Seq(Sequence.Output(None, InputNoArgs("1", 1), ExpectingDescription("1"), Cut.False)),
				new ConstFailure(ExpectingDescription("right"), Cut.False),
				Failure(
					OrTrace(
						LeafTrace(ExpectingDescription("1"), initialInput),
						LeafTrace(ExpectingDescription("right"), initialInput)
					),
					Cut.False
				)
			),
			(
				"If the repeat is permissibly empty, and right fails with cut, then fails and shows the cut as expected",
				(0, Integer.MAX_VALUE),
				Seq(Sequence.Output(None, InputNoArgs("1", 1), ExpectingDescription("1"), Cut.False)),
				new ConstFailure(ExpectingDescription("right"), Cut.True),
				Failure(
					LeafTrace(ExpectingDescription("right"), initialInput),
					Cut.True
				)
			),
			(
				"If the repeat is permissibly empty, and right succeeds, then result is success",
				(0, Integer.MAX_VALUE),
				Seq(Sequence.Output(None, InputNoArgs("1", 1), ExpectingDescription("1"), Cut.False)),
				new ConstSuccess(SuccessValue(0xCAFE), InputNoArgs("right", 0xCAFE), ExpectingDescription("right"), Cut.False),
				Success(
					(Seq.empty, SuccessValue(0xCAFE)),
					InputNoArgs("right", 0xCAFE),
					LeafTrace(ExpectingDescription("right"), initialInput),
					Cut.False
				)
			),
			(
				"If the repeat is a failed cut, then the result matches that failure",
				(0, Integer.MAX_VALUE),
				Seq(Sequence.Output(None, InputNoArgs("1", 1), ExpectingDescription("1"), Cut.True)),
				new ConstSuccess(SuccessValue(0xCAFE), InputNoArgs("right", 0xCAFE), ExpectingDescription("right"), Cut.False),
				Failure(
					LeafTrace(ExpectingDescription("1"), initialInput),
					Cut.True
				)
			),
			(
				"If the repeat has a failed cut, then the result matches that failure",
				(0, Integer.MAX_VALUE),
				Seq(Sequence.Output(None, InputNoArgs("1", 1), ExpectingDescription("1"), Cut.True)),
				new ConstSuccess(SuccessValue(0xCAFE), InputNoArgs("right", 0xCAFE), ExpectingDescription("right"), Cut.False),
				Failure(
					LeafTrace(ExpectingDescription("1"), initialInput),
					Cut.True
				)
			)
		)

		cases.foreach({case (name, repeatBounds, leftSeq, rightParser, expected) =>
			it (name) {
				val leftParser = new Sequence(initialInput, leftSeq).repeat(repeatBounds._1, repeatBounds._2)
				val parser = leftParser andThen rightParser
				assertResult(expected){parser.parse(initialInput)}
			}
		})

		it ("missing right with any repeat fails and shows both inputs as options") {
			val initialInput = InputNoArgs("a", 42)
			val leftParser = CharIn("a").repeat()
			val rightParser = CharIn("b")
			val leftExpecting = ExpectingDescription("CharIn(\"a\")")
			val rightExpecting = ExpectingDescription("CharIn(\"b\")")

			val expected = Failure(
				ThenTrace(
					LeafTrace(leftExpecting, initialInput),
					OrTrace(
						LeafTrace(leftExpecting, InputNoArgs("", 43)),
						LeafTrace(rightExpecting, InputNoArgs("", 43))
					)
				),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("unexpected right with any repeat fails and shows both inputs as options") {
			val initialInput = InputNoArgs("ac", 42)
			val leftParser = CharIn("a").repeat()
			val rightParser = CharIn("b")
			val leftExpecting = ExpectingDescription("CharIn(\"a\")")
			val rightExpecting = ExpectingDescription("CharIn(\"b\")")

			val expected = Failure(
				ThenTrace(
					LeafTrace(leftExpecting, initialInput),
					OrTrace(
						LeafTrace(leftExpecting, InputNoArgs("c", 43)),
						LeafTrace(rightExpecting, InputNoArgs("c", 43))
					)
				),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("input too short for repeat") {
			val initialInput = InputNoArgs("a", 42)
			val leftParser = CharIn("a").repeat(3,5)
			val rightParser = CharIn("b")
			val leftExpecting = ExpectingDescription("CharIn(\"a\")")

			val expected = Failure(
				ThenTrace(
					LeafTrace(leftExpecting, initialInput),
					LeafTrace(leftExpecting, InputNoArgs("", 43))
				),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("input too long for repeat") {
			val initialInput = InputNoArgs("aaaaaaaa", 42)
			val leftParser = CharIn("a").repeat(3,5)
			val rightParser = CharIn("b")
			val leftExpecting = ExpectingDescription("CharIn(\"a\")")
			val rightExpecting = ExpectingDescription("CharIn(\"b\")")

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
				),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("pattern.repeat andThen subset") {
			val initialInput = InputNoArgs("aa", 42)
			val leftParser = CharIn("ab").repeat()
			val rightParser = CharIn("a")
			val leftExpecting = ExpectingDescription("CharIn(\"ab\")")
			val rightExpecting = ExpectingDescription("CharIn(\"a\")")

			val expected = Success(
				("a", 'a'),
				InputNoArgs("", 44),
				ThenTrace(
					LeafTrace(leftExpecting, InputNoArgs("aa", 42)),
					LeafTrace(rightExpecting, InputNoArgs("a", 43))
				),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("pattern.repeat andThen subset twice") {
			val initialInput = InputNoArgs("aaa", 42)
			val leftParser = CharIn("ab").repeat()
			val rightParser = CharIn("a")
			val leftExpecting = ExpectingDescription("CharIn(\"ab\")")
			val rightExpecting = ExpectingDescription("CharIn(\"a\")")

			val expected = Success(
				("aa", 'a'),
				InputNoArgs("", 45),
				ThenTrace(
					ThenTrace(
						LeafTrace(leftExpecting, InputNoArgs("aaa", 42)),
						LeafTrace(leftExpecting, InputNoArgs("aa", 43))
					),
					LeafTrace(rightExpecting, InputNoArgs("a", 44))
				),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("does not backtrack across a cut") {
			val initialInput = InputNoArgs("abcd", 42)

			val leftLeftParser = CharIn[Nothing]("a") andThenWithCut CharIn[Nothing]("b")
			val leftRightParser = CharIn[Nothing]("c") andThen CharIn[Nothing]("d")
			val leftParser = (leftLeftParser orElse leftRightParser).repeat()
			val rightParser = IsString("abcd")

			val expected = Failure(
				ThenTrace(
					ThenTrace(
						ThenTrace(
							LeafTrace(ExpectingDescription("CharIn(\"a\")"), InputNoArgs("abcd", 42)),
							LeafTrace(ExpectingDescription("CharIn(\"b\")"), InputNoArgs("bcd", 43))
						),
						ThenTrace(
							LeafTrace(ExpectingDescription("CharIn(\"c\")"), InputNoArgs("cd", 44)),
							LeafTrace(ExpectingDescription("CharIn(\"d\")"), InputNoArgs("d", 45))
						)
					),
					OrTrace(
						OrTrace(
							LeafTrace(ExpectingDescription("CharIn(\"a\")"), InputNoArgs("", 46)),
							LeafTrace(ExpectingDescription("CharIn(\"c\")"), InputNoArgs("", 46))
						),
						LeafTrace(ExpectingDescription("\"abcd\""), InputNoArgs("", 46))
					)
				),
				Cut.True
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("unexpected right with any repeat and delimiter fails and shows delimiter and right as options") {
			val initialInput = InputNoArgs("az", 42)
			val leftParser = CharIn("a").opaque("rep").repeat(delimiter = IsString("b").map(_ => ()).opaque("delim"))
			val rightParser = CharIn("c").opaque("right")

			val expected = Failure(
				ThenTrace(
					LeafTrace(ExpectingDescription("rep"), initialInput),
					OrTrace(
						LeafTrace(ExpectingDescription("delim"), InputNoArgs("z", 43)),
						LeafTrace(ExpectingDescription("right"), InputNoArgs("z", 43))
					)
				),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("successful with delimiter") {
			val initialInput = InputNoArgs("abac", 42)
			val leftParser = CharIn("a").opaque("rep").repeat(delimiter = IsString("b").map(_ => ()).opaque("delim"))
			val rightParser = CharIn("c").opaque("right")

			val expected = Success(
				("aa", 'c'),
				InputNoArgs("", 46),
				ThenTrace(
					ThenTrace(
						ThenTrace(
							LeafTrace(ExpectingDescription("rep"), initialInput),
							LeafTrace(ExpectingDescription("delim"), InputNoArgs("bac", 43))
						),
						LeafTrace(ExpectingDescription("rep"), InputNoArgs("ac", 44))
					),
					LeafTrace(ExpectingDescription("right"), InputNoArgs("c", 45))
				),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
	}
}
