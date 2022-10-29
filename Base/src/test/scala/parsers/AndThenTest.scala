package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec
import TestUtilities._

final class AndThenTest extends AnyFunSpec {
	describe ("AndThen") {
		it ("if both childs are successful, then reports success") {
			val initialInput = SinglePartInput("1234", 42)
			val middleInput = SinglePartInput("abcd", 24)
			val endInput = SinglePartInput("wxyz", 13)

			val leftResult = new Object
			val rightResult = new Object
			val leftExpect = SingleExpecting("Left", 101)
			val rightExpect = SingleExpecting("Right", 102)

			val leftParser = new ConstSuccess(leftResult, middleInput, leftExpect, Cut.False)
			val rightParser = new ConstSuccess(rightResult, endInput, rightExpect, Cut.False)

			val expected = Success[Nothing, StubPosition, (Object, Object)](
				(leftResult, rightResult),
				endInput,
				leftExpect ++ rightExpect,
				Cut.False
			)
			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("if the first child is failure, then forwards that failure") {
			val initialInput = new Input[Nothing, StubPosition](("1234", StubPosition(42)) :: Nil, Nil)

			val leftExpect = SingleExpecting("Left", 101)

			val leftParser = new ConstFailure(leftExpect, Cut.False)
			val rightParser = new ConstSuccess(new Object, new Input[Nothing, StubPosition](("wxyz", StubPosition(13)) :: Nil, Nil), Set.empty, Cut.False)

			val expected = Failure(leftExpect, Cut.False)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("if the first child is success and second child is failure, then reports a failure that includes the first child in the trace") {
			val initialInput = SinglePartInput("1234", 42)
			val middleInput = SinglePartInput("abcd", 151)

			val leftResult = new Object
			val leftExpect = SingleExpecting("Left", 101)
			val rightExpect = SingleExpecting("Right", 102)

			val leftParser = new ConstSuccess(leftResult, middleInput, leftExpect, Cut.False)
			val rightParser = new ConstFailure(rightExpect, Cut.False)

			val expected = Failure(
				leftExpect ++ rightExpect,
				Cut.False
			)

			val parser = leftParser andThen rightParser
			assertResult(expected){parser.parse(initialInput)}
		}
	}
}
