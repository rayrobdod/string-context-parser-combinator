package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec
import com.rayrobdod.stringContextParserCombinator.RepeatStrategy._
import TestUtilities._

final class AndThen_Repeat_Test extends AnyFunSpec {
	implicit object RepeatedNothing extends typelevel.Repeated[Nothing, Unit] {
		type Acc = Unit
		def init():Acc = ()
		def append(acc:Acc, elem:Nothing):Unit = ()
		def result(acc:Acc):Unit = ()
	}

	describe ("AndThen / Repeat") {
		val initialInput = SinglePartInput("initial", 1234)

		it ("If the repeat is permissibly empty and right fails, then fails and shows both inputs as expected options") {
			val left = new ConstFailure(SingleExpecting("left", 5), Cut.False)
			val right = new ConstFailure(SingleExpecting("right", 100), Cut.False)
			val expected = Failure(
				SingleExpecting("left", 5) ++ SingleExpecting("right", 100),
				Cut.False
			)

			val parser = left.repeat(strategy = Greedy) andThen right
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("If the repeat is permissibly empty, and right fails with cut, then fails and shows the cut as expected") {
			val left = new ConstFailure(SingleExpecting("left", 5), Cut.False)
			val right = new ConstFailure(SingleExpecting("right", 100), Cut.True)
			val expected = Failure(
				SingleExpecting("right", 100),
				Cut.True
			)

			val parser = left.repeat(strategy = Greedy) andThen right
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("If the repeat is permissibly empty, and right succeeds, then result is success") {
			val left = new ConstFailure(SingleExpecting("left", 5), Cut.False)
			val right = new ConstSuccess("Value", SinglePartInput("rest", 0), SingleExpecting("right", 100), Cut.False)
			val expected = Success(
				"Value",
				SinglePartInput("rest", 0),
				SingleExpecting("left", 5) ++ SingleExpecting("right", 100),
				Cut.False
			)

			val parser = left.repeat(strategy = Greedy) andThen right
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("If the repeat has a failed cut, then the result matches that failure") {
			val left = new ConstFailure(SingleExpecting("left", 5), Cut.True)
			val right = new ConstFailure(SingleExpecting("right", 100), Cut.False)
			val expected = Failure(
				SingleExpecting("left", 5),
				Cut.True
			)

			val parser = left.repeat(strategy = Greedy) andThen right
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("missing right with any repeat fails and shows both inputs as options") {
			val initialInput = SinglePartInput("a", 42)
			val leftParser = CharIn("a").repeat(strategy = Greedy)
			val rightParser = CharIn("b")

			val expected = Failure(
				RepeatedExpecting("CharIn(\"a\")", 42 to 43) ++
					RepeatedExpecting("CharIn(\"b\")", 42 to 43),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("unexpected right with any repeat fails and shows both inputs as options") {
			val initialInput = SinglePartInput("ac", 42)
			val leftParser = CharIn("a").repeat(strategy = Greedy)
			val rightParser = CharIn("b")

			val expected = Failure(
				RepeatedExpecting("CharIn(\"a\")", 42 to 43) ++
					RepeatedExpecting("CharIn(\"b\")", 42 to 43),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("input too short for repeat") {
			val initialInput = SinglePartInput("a", 42)
			val leftParser = CharIn("a").repeat(3,5, strategy = Greedy)
			val rightParser = CharIn("b")

			val expected = Failure(
				RepeatedExpecting("CharIn(\"a\")", 42 to 43),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("input too long for repeat") {
			val initialInput = SinglePartInput("aaaaaaaa", 42)
			val leftParser = CharIn("a").repeat(3,5, strategy = Greedy)
			val rightParser = CharIn("b")
			val rightExpecting = SingleExpecting("CharIn(\"b\")", 47)

			val expected = Failure(
				RepeatedExpecting("CharIn(\"a\")", 42 to 46) ++
					RepeatedExpecting("CharIn(\"b\")", 45 to 47),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("pattern.repeat(Greedy) andThen subset") {
			val initialInput = SinglePartInput("aa", 42)
			val leftParser = CharIn("ab").repeat(strategy = Greedy)
			val rightParser = CharIn("a")
			val leftExpecting = SingleExpecting("CharIn(\"ab\")", 44)
			val rightExpecting = SingleExpecting("CharIn(\"a\")", 44)

			val expected = Success(
				Success1(
					("a", 'a'),
					SinglePartInput("", 44),
					SingleExpecting("CharIn(\"ab\")", 42) ++
						SingleExpecting("CharIn(\"a\")", 43),
					Cut.False
				),
				List(
					Success1(
						("", 'a'),
						SinglePartInput("a", 43),
						SingleExpecting("CharIn(\"a\")", 42),
						Cut.False
					)
				)
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("pattern.repeat andThen subset twice") {
			val initialInput = SinglePartInput("aaa", 42)
			val leftParser = CharIn("ab").repeat(strategy = Greedy)
			val rightParser = CharIn("a")
			val leftExpecting = SingleExpecting("CharIn(\"ab\")", 45)
			val rightExpecting = SingleExpecting("CharIn(\"a\")", 45)

			val expected = Success(
				Success1(
					("aa", 'a'),
					SinglePartInput("", 45),
					SingleExpecting("CharIn(\"ab\")", 42) ++
						SingleExpecting("CharIn(\"ab\")", 43) ++
						SingleExpecting("CharIn(\"a\")", 44),
					Cut.False
				),
				List(
					Success1(
						("a", 'a'),
						SinglePartInput("a", 44),
						SingleExpecting("CharIn(\"ab\")", 42) ++
							SingleExpecting("CharIn(\"a\")", 43),
						Cut.False
					),
					Success1(
						("", 'a'),
						SinglePartInput("aa", 43),
						SingleExpecting("CharIn(\"a\")", 42),
						Cut.False
					)
				)
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("pattern.repeat(Possessive) andThen subset fails") {
			val initialInput = SinglePartInput("aa", 42)
			val leftParser = CharIn("ab").repeat(strategy = Possessive)
			val rightParser = CharIn("a")
			val leftExpecting = SingleExpecting("CharIn(\"ab\")", 44)
			val rightExpecting = SingleExpecting("CharIn(\"a\")", 44)

			val expected = Failure(
				SingleExpecting("CharIn(\"ab\")", 42) ++
					SingleExpecting("CharIn(\"ab\")", 43) ++
					SingleExpecting("CharIn(\"ab\")", 44) ++
					SingleExpecting("CharIn(\"a\")", 44),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("does not backtrack across a cut") {
			val initialInput = SinglePartInput("abcd", 42)

			val leftLeft = new ConstSuccess("XXX", SinglePartInput("leftLeft", 8), SingleExpecting("leftLeft", 4), Cut.False)
			val leftRight = new ConstFailure(SingleExpecting("leftRight", 8), Cut.False)
			val right = new ConstSuccess("XXX", SinglePartInput("right", 12), SingleExpecting("right", 12), Cut.False)

			val expected = Failure(
				SingleExpecting("leftRight", 8),
				Cut.True
			)

			val parser = (leftLeft andThenWithCut leftRight).repeat(strategy = Greedy) andThen right
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("unexpected right with any repeat and delimiter fails and shows delimiter and right as options") {
			val initialInput = SinglePartInput("az", 42)
			val leftParser = CharIn("a").opaque("rep").repeat(delimiter = IsString("b").map(_ => ()).opaque("delim"), strategy = Greedy)
			val rightParser = CharIn("c").opaque("right")

			val expected = Failure(
				SingleExpecting("rep", 42) ++
					SingleExpecting("right", 42) ++
					SingleExpecting("delim", 43) ++
					SingleExpecting("right", 43),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("successful with delimiter") {
			val initialInput = SinglePartInput("abac", 42)
			val leftParser = CharIn("a").opaque("rep").repeat(delimiter = IsString("b").map(_ => ()).opaque("delim"), strategy = Greedy)
			val rightParser = CharIn("c").opaque("right")

			val expected = Success(
				("aa", 'c'),
				SinglePartInput("", 46),
				SingleExpecting("rep", 42) ++
					SingleExpecting("delim", 43) ++
					SingleExpecting("rep", 44) ++
					SingleExpecting("right", 45) ++
					SingleExpecting("delim", 45),
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("right associative variant of \"'[' ~ 'a'.rep(',') ~ ']' reports the delimiter as an option when the suffix is not found\"") {
			val initialInput = SinglePartInput("[a:a]", 42)
			val parser = IsString[Nothing]("[") andThen (IsString[Nothing]("a").repeat(delimiter = IsString[Nothing](","), strategy = Greedy) andThen IsString[Nothing]("]"))

			val expected = Failure(
				SingleExpecting("\",\"",44) ++
					SingleExpecting("\"]\"",44) ++
					SingleExpecting("\"]\"",43) ++
					SingleExpecting("\"a\"",43) ++
					SingleExpecting("\"[\"",42),
				Cut.False
			)

			assertResult(expected){parser.parse(initialInput)}
		}
		it ("'[' ~ 'a'.rep(',') ~ ']' reports the delimiter as an option when the suffix is not found") {
			val initialInput = SinglePartInput("[a:a]", 42)
			val parser = IsString[Nothing]("[") andThen IsString[Nothing]("a").repeat(delimiter = IsString[Nothing](","), strategy = Greedy) andThen IsString[Nothing]("]")

			val expected = Failure(
				SingleExpecting("\",\"",44) ++
					SingleExpecting("\"]\"",44) ++
					SingleExpecting("\"]\"",43) ++
					SingleExpecting("\"a\"",43) ++
					SingleExpecting("\"[\"",42),
				Cut.False
			)

			assertResult(expected){parser.parse(initialInput)}
		}
		it ("right associative variant of \"'[' ~/ 'a'.rep(',') ~ ']' reports the delimiter as an option when the suffix is not found\"") {
			val initialInput = SinglePartInput("[a:a]", 42)
			val parser = IsString[Nothing]("[") andThenWithCut (IsString[Nothing]("a").repeat(delimiter = IsString[Nothing](","), strategy = Greedy) andThen IsString[Nothing]("]"))

			val expected = Failure(
				SingleExpecting("\",\"",44) ++
					SingleExpecting("\"]\"",44) ++
					SingleExpecting("\"]\"",43) ++
					SingleExpecting("\"a\"",43),
				Cut.True
			)

			assertResult(expected){parser.parse(initialInput)}
		}
		it ("'[' ~/ 'a'.rep(',') ~ ']' reports the delimiter as an option when the suffix is not found") {
			val initialInput = SinglePartInput("[a:a]", 42)
			val parser = IsString[Nothing]("[") andThenWithCut IsString[Nothing]("a").repeat(delimiter = IsString[Nothing](","), strategy = Greedy) andThen IsString[Nothing]("]")

			val expected = Failure(
				SingleExpecting("\",\"",44) ++
					SingleExpecting("\"]\"",44) ++
					SingleExpecting("\"]\"",43) ++
					SingleExpecting("\"a\"",43),
				Cut.True
			)

			assertResult(expected){parser.parse(initialInput)}
		}
	}
}
