package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec

final class OrElseTest extends AnyFunSpec {
	describe("OrElse") {
		it("`Success | Whatever` returns that success") {
			val initialInput = new Input[Nothing](("1234", PositionPoint(42)) :: Nil, Nil)
			val leftEndInput = new Input[Nothing](("wxyz", PositionPoint(151)) :: Nil, Nil)
			val rightEndInput = new Input[Nothing](("wxyz", PositionPoint(151)) :: Nil, Nil)

			val leftResult = new Object
			val rightResult = new Object
			val leftExpect = Expecting("Left")
			val rightExpect = Expecting("Right")

			val leftParser = new ConstSuccess(leftResult, leftEndInput, leftExpect, Cut.False)
			val rightParser = new ConstSuccess(rightResult, rightEndInput, rightExpect, Cut.False)

			val expected = Success[Nothing, Object](
				leftResult,
				leftEndInput,
				LeafTrace(leftExpect, initialInput),
				Cut.False
			)
			val parser = leftParser orElse rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it("`Cut | Whatever` returns that failure") {
			val initialInput = new Input[Nothing](("1234", PositionPoint(42)) :: Nil, Nil)
			val rightEndInput = new Input[Nothing](("wxyz", PositionPoint(151)) :: Nil, Nil)

			val rightResult = new Object
			val leftExpect = Expecting("Left")
			val rightExpect = Expecting("Right")

			val leftParser = new ConstFailure(leftExpect, Cut.True)
			val rightParser = new ConstSuccess(rightResult, rightEndInput, rightExpect, Cut.False)

			val expected = Failure[Nothing](
				LeafTrace(leftExpect, initialInput),
				Cut.True
			)
			val parser = leftParser orElse rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it("`Failure | Success` returns that success") {
			val initialInput = new Input[Nothing](("1234", PositionPoint(42)) :: Nil, Nil)
			val rightEndInput = new Input[Nothing](("wxyz", PositionPoint(151)) :: Nil, Nil)

			val rightResult = new Object
			val leftExpect = Expecting("Left")
			val rightExpect = Expecting("Right")

			val leftParser = new ConstFailure(leftExpect, Cut.False)
			val rightParser = new ConstSuccess(rightResult, rightEndInput, rightExpect, Cut.False)

			val expected = Success[Nothing, Object](
				rightResult,
				rightEndInput,
				LeafTrace(rightExpect, initialInput),
				Cut.False
			)
			val parser = leftParser orElse rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it("`Failure | Cut` returns a failure that mentions only the cut branch") {
			val initialInput = new Input[Nothing](("1234", PositionPoint(42)) :: Nil, Nil)

			val leftExpect = Expecting("Left")
			val rightExpect = Expecting("Right")

			val leftParser = new ConstFailure(leftExpect, Cut.False)
			val rightParser = new ConstFailure(rightExpect, Cut.True)

			val expected = Failure[Nothing](
				LeafTrace(rightExpect, initialInput),
				Cut.True
			)
			val parser = leftParser orElse rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it("`Failure | Failure` returns a failure that mentions both branches") {
			val initialInput = new Input[Nothing](("1234", PositionPoint(42)) :: Nil, Nil)

			val leftExpect = Expecting("Left")
			val rightExpect = Expecting("Right")

			val leftParser = new ConstFailure(leftExpect, Cut.False)
			val rightParser = new ConstFailure(rightExpect, Cut.False)

			val expected = Failure[Nothing](
				OrTrace(
					LeafTrace(leftExpect, initialInput),
					LeafTrace(rightExpect, initialInput)
				),
				Cut.False
			)
			val parser = leftParser orElse rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
	}
}
