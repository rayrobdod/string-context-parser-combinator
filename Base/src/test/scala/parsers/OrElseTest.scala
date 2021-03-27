package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec

final class OrElseTest extends AnyFunSpec {
	val positionOfNothing:Nothing => Position = x => x
	def SinglePartInput(str:String, pos:Int) = new Input[Nothing](((str, Position(pos))) :: Nil, Nil, positionOfNothing)
	def SingleExpecting(msg:String, pos:Int) = Set(Expecting(msg, Position(pos)))

	describe("OrElse") {
		it("`Success | Whatever` returns that success") {
			val initialInput = SinglePartInput("1234", 42)
			val leftEndInput = SinglePartInput("wxyz", 151)
			val rightEndInput = SinglePartInput("wxyz", 151)

			val leftResult = new Object
			val rightResult = new Object
			val leftExpect = SingleExpecting("Left", 101)
			val rightExpect = SingleExpecting("Right", 102)

			val leftParser = new ConstSuccess(leftResult, leftEndInput, leftExpect, Cut.False)
			val rightParser = new ConstSuccess(rightResult, rightEndInput, rightExpect, Cut.False)

			val expected = Success[Nothing, Object](
				leftResult,
				leftEndInput,
				leftExpect,
				Cut.False
			)
			val parser = leftParser orElse rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it("`Cut | Whatever` returns that failure") {
			val initialInput = SinglePartInput("1234", 42)
			val rightEndInput = SinglePartInput("wxyz", 151)

			val rightResult = new Object
			val leftExpect = SingleExpecting("Left", 101)
			val rightExpect = SingleExpecting("Right", 102)

			val leftParser = new ConstFailure(leftExpect, Cut.True)
			val rightParser = new ConstSuccess(rightResult, rightEndInput, rightExpect, Cut.False)

			val expected = Failure(
				leftExpect,
				Cut.True
			)
			val parser = leftParser orElse rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it("`Failure | Success` returns that success") {
			val initialInput = SinglePartInput("1234", 42)
			val rightEndInput = SinglePartInput("wxyz", 151)

			val rightResult = new Object
			val leftExpect = SingleExpecting("Left", 101)
			val rightExpect = SingleExpecting("Right", 102)

			val leftParser:Parser[Nothing, Object] = new ConstFailure(leftExpect, Cut.False)
			val rightParser:Parser[Nothing, Object] = new ConstSuccess(rightResult, rightEndInput, rightExpect, Cut.False)

			val expected = Success[Nothing, Object](
				rightResult,
				rightEndInput,
				rightExpect,
				Cut.False
			)
			val parser = leftParser orElse rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it("`Failure | Cut` returns a failure that mentions only the cut branch") {
			val initialInput = SinglePartInput("1234", 42)

			val leftExpect = SingleExpecting("Left", 101)
			val rightExpect = SingleExpecting("Right", 102)

			val leftParser = new ConstFailure(leftExpect, Cut.False)
			val rightParser = new ConstFailure(rightExpect, Cut.True)

			val expected = Failure(
				rightExpect,
				Cut.True
			)
			val parser = leftParser orElse rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it("`Failure | Failure` returns a failure that mentions both branches") {
			val initialInput = SinglePartInput("1234", 42)

			val leftExpect = SingleExpecting("Left", 101)
			val rightExpect = SingleExpecting("Right", 102)

			val leftParser = new ConstFailure(leftExpect, Cut.False)
			val rightParser = new ConstFailure(rightExpect, Cut.False)

			val expected = Failure(
				leftExpect ++ rightExpect,
				Cut.False
			)
			val parser = leftParser orElse rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
	}
}
