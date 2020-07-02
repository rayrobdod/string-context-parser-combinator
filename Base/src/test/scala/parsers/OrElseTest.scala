package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec

final class OrElseTest extends AnyFunSpec {
	describe("OrElse") {
		it("if the first child is success, then forwards that success") {
			val initialInput = new Input[Nothing](("1234", PositionPoint(42)) :: Nil, Nil)
			val leftEndInput = new Input[Nothing](("wxyz", PositionPoint(151)) :: Nil, Nil)
			val rightEndInput = new Input[Nothing](("wxyz", PositionPoint(151)) :: Nil, Nil)

			val leftResult = new Object
			val rightResult = new Object
			val leftExpect = Expecting("Left")
			val rightExpect = Expecting("Right")

			val leftParser = new ConstSuccess(leftResult, leftEndInput, leftExpect)
			val rightParser = new ConstSuccess(rightResult, rightEndInput, rightExpect)

			val expected = Success[Nothing, Object](
				leftResult,
				leftEndInput,
				LeafTrace(leftExpect, initialInput)
			)
			val parser = leftParser orElse rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it("if the first child is failure and the second child is success, then forwards that success") {
			val initialInput = new Input[Nothing](("1234", PositionPoint(42)) :: Nil, Nil)
			val rightEndInput = new Input[Nothing](("wxyz", PositionPoint(151)) :: Nil, Nil)

			val rightResult = new Object
			val leftExpect = Expecting("Left")
			val rightExpect = Expecting("Right")

			val leftParser = new ConstFailure(leftExpect)
			val rightParser = new ConstSuccess(rightResult, rightEndInput, rightExpect)

			val expected = Success[Nothing, Object](
				rightResult,
				rightEndInput,
				LeafTrace(rightExpect, initialInput)
			)
			val parser = leftParser orElse rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it("if both childs are failure, then returns a failure either either expect") {
			val initialInput = new Input[Nothing](("1234", PositionPoint(42)) :: Nil, Nil)

			val leftExpect = Expecting("Left")
			val rightExpect = Expecting("Right")

			val leftParser = new ConstFailure(leftExpect)
			val rightParser = new ConstFailure(rightExpect)

			val expected = Failure[Nothing](
				OrTrace(
					LeafTrace(leftExpect, initialInput),
					LeafTrace(rightExpect, initialInput)
				)
			)
			val parser = leftParser orElse rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
	}
}
