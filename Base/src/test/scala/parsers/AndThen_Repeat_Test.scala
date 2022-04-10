package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec

final class AndThen_Repeat_Test extends AnyFunSpec {
	final case class SuccessValue(x:Int)
	def InputPart(str:String, pos:Int) = ((str, Position(pos)))
	val positionOfNothing:Nothing => Position = x => x
	def InputNoArgs(str:String, pos:Int) = new Input[Nothing](InputPart(str, pos) :: Nil, Nil, positionOfNothing)
	def SingleExpecting(msg:String, pos:Int) = Set(Expecting(ExpectingDescription(msg), Position(pos)))

	implicit object RepeatedNothing extends typelevel.Repeated[Nothing, Unit] {
		type Acc = Unit
		def init():Acc = ()
		def append(acc:Acc, elem:Nothing):Unit = ()
		def result(acc:Acc):Unit = ()
	}

	describe ("AndThen / Repeat") {
		val initialInput = InputNoArgs("initial", 1234)

		it ("If the repeat is permissibly empty and right fails, then fails and shows both inputs as expected options") {
			val left = new ConstFailure(SingleExpecting("left", 5), Cut.False)
			val right = new ConstFailure(SingleExpecting("right", 100), Cut.False)
			val expected = Failure(
				SingleExpecting("left", 5) ++ SingleExpecting("right", 100),
				Cut.False
			)

			val parser = left.repeat() andThen right
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("If the repeat is permissibly empty, and right fails with cut, then fails and shows the cut as expected") {
			val left = new ConstFailure(SingleExpecting("left", 5), Cut.False)
			val right = new ConstFailure(SingleExpecting("right", 100), Cut.True)
			val expected = Failure(
				SingleExpecting("right", 100),
				Cut.True
			)

			val parser = left.repeat() andThen right
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("If the repeat is permissibly empty, and right succeeds, then result is success") {
			val left = new ConstFailure(SingleExpecting("left", 5), Cut.False)
			val right = new ConstSuccess("Value", InputNoArgs("rest", 0), SingleExpecting("right", 100), Cut.False)
			val expected = Success(
				"Value",
				InputNoArgs("rest", 0),
				SingleExpecting("left", 5) ++ SingleExpecting("right", 100),
				Cut.False
			)

			val parser = left.repeat() andThen right
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("If the repeat has a failed cut, then the result matches that failure") {
			val left = new ConstFailure(SingleExpecting("left", 5), Cut.True)
			val right = new ConstFailure(SingleExpecting("right", 100), Cut.False)
			val expected = Failure(
				SingleExpecting("left", 5),
				Cut.True
			)

			val parser = left.repeat() andThen right
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("missing right with any repeat fails and shows both inputs as options") {
			val initialInput = InputNoArgs("a", 42)
			val leftParser = CharIn("a").repeat()
			val rightParser = CharIn("b")
			val leftExpecting = SingleExpecting("CharIn(\"a\")", 43)
			val rightExpecting = SingleExpecting("CharIn(\"b\")", 43)

			val expected = Failure(
				leftExpecting ++ rightExpecting ++ SingleExpecting("CharIn(\"b\")", 42),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("unexpected right with any repeat fails and shows both inputs as options") {
			val initialInput = InputNoArgs("ac", 42)
			val leftParser = CharIn("a").repeat()
			val rightParser = CharIn("b")
			val leftExpecting = SingleExpecting("CharIn(\"a\")", 43)
			val rightExpecting = SingleExpecting("CharIn(\"b\")", 43)

			val expected = Failure(
				leftExpecting ++ rightExpecting ++ SingleExpecting("CharIn(\"b\")", 42),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("input too short for repeat") {
			val initialInput = InputNoArgs("a", 42)
			val leftParser = CharIn("a").repeat(3,5)
			val rightParser = CharIn("b")
			val leftExpecting = SingleExpecting("CharIn(\"a\")", 43)

			val expected = Failure(
				leftExpecting,
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("input too long for repeat") {
			val initialInput = InputNoArgs("aaaaaaaa", 42)
			val leftParser = CharIn("a").repeat(3,5)
			val rightParser = CharIn("b")
			val rightExpecting = SingleExpecting("CharIn(\"b\")", 47)

			val expected = Failure(
				rightExpecting ++ SingleExpecting("CharIn(\"b\")", 46) ++ SingleExpecting("CharIn(\"b\")", 45),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("pattern.repeat andThen subset") {
			val initialInput = InputNoArgs("aa", 42)
			val leftParser = CharIn("ab").repeat()
			val rightParser = CharIn("a")
			val leftExpecting = SingleExpecting("CharIn(\"ab\")", 44)
			val rightExpecting = SingleExpecting("CharIn(\"a\")", 44)

			val expected = Success(
				Success1(
					("a", 'a'),
					InputNoArgs("", 44),
					Set.empty,
					Cut.False
				),
				List(
					Success1(
						("", 'a'),
						InputNoArgs("a", 43),
						Set.empty,
						Cut.False
					)
				)
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("pattern.repeat andThen subset twice") {
			val initialInput = InputNoArgs("aaa", 42)
			val leftParser = CharIn("ab").repeat()
			val rightParser = CharIn("a")
			val leftExpecting = SingleExpecting("CharIn(\"ab\")", 45)
			val rightExpecting = SingleExpecting("CharIn(\"a\")", 45)

			val expected = Success(
				Success1(
					("aa", 'a'),
					InputNoArgs("", 45),
					Set.empty,
					Cut.False
				),
				List(
					Success1(
						("a", 'a'),
						InputNoArgs("a", 44),
						Set.empty,
						Cut.False
					),
					Success1(
						("", 'a'),
						InputNoArgs("aa", 43),
						Set.empty,
						Cut.False
					)
				)
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("does not backtrack across a cut") {
			val initialInput = InputNoArgs("abcd", 42)

			val leftLeft = new ConstSuccess("XXX", InputNoArgs("leftLeft", 8), SingleExpecting("leftLeft", 4), Cut.False)
			val leftRight = new ConstFailure(SingleExpecting("leftRight", 8), Cut.False)
			val right = new ConstSuccess("XXX", InputNoArgs("right", 12), SingleExpecting("right", 12), Cut.False)

			val expected = Failure(
				SingleExpecting("leftRight", 8),
				Cut.True
			)

			val parser = (leftLeft andThenWithCut leftRight).repeat() andThen right
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("unexpected right with any repeat and delimiter fails and shows delimiter and right as options") {
			val initialInput = InputNoArgs("az", 42)
			val leftParser = CharIn("a").opaque("rep").repeat(delimiter = IsString("b").map(_ => ()).opaque("delim"))
			val rightParser = CharIn("c").opaque("right")

			val expected = Failure(
				SingleExpecting("delim", 43) ++ SingleExpecting("right", 43) ++ SingleExpecting("right", 42),
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
				SingleExpecting("delim", 45),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
	}
}
