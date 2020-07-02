package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec

final class AndThenTest extends AnyFunSpec {
	describe ("AndThen") {
		it ("if both childs are successful, then reports success") {
			val initialInput = new Input[Nothing](("1234", PositionPoint(42)) :: Nil, Nil)
			val middleInput = new Input[Nothing](("abcd", PositionPoint(24)) :: Nil, Nil)
			val endInput = new Input[Nothing](("wxyz", PositionPoint(13)) :: Nil, Nil)

			val leftResult = new Object
			val rightResult = new Object
			val leftExpect = Expecting("Left")
			val rightExpect = Expecting("Right")

			val leftParser = new ConstSuccess(leftResult, middleInput, leftExpect)
			val rightParser = new ConstSuccess(rightResult, endInput, rightExpect)

			val expected = Success[Nothing, (Object, Object)](
				(leftResult, rightResult),
				endInput,
				ThenTrace(
					LeafTrace(leftExpect, initialInput),
					LeafTrace(rightExpect, middleInput)
				)
			)
			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("if the first child is failure, then forwards that failure") {
			val initialInput = new Input[Nothing](("1234", PositionPoint(42)) :: Nil, Nil)

			val leftExpect = Expecting("Left")

			val leftParser = new ConstFailure(leftExpect)
			val rightParser = new ConstSuccess(new Object, new Input[Nothing](("wxyz", PositionPoint(13)) :: Nil, Nil), Expecting("Right"))

			val expected = Failure[Nothing](LeafTrace(leftExpect, initialInput))

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("if the first child is success and second child is failure, then reports a failure that includes the first child in the trace") {
			val initialInput = new Input[Nothing](("1234", PositionPoint(42)) :: Nil, Nil)
			val middleInput = new Input[Nothing](("abcd", PositionPoint(151)) :: Nil, Nil)

			val leftResult = new Object
			val leftExpect = Expecting("Left")
			val rightExpect = Expecting("Right")

			val leftParser = new ConstSuccess(leftResult, middleInput, leftExpect)
			val rightParser = new ConstFailure(rightExpect)

			val expected = Failure[Nothing](ThenTrace(LeafTrace(leftExpect, initialInput), LeafTrace(rightExpect, middleInput)))

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
	}
}
