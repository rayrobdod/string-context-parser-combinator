package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec

final class OrElse_AndThenWithCut_Test extends AnyFunSpec {
	def InputPart(str:String, pos:Int) = ((str, Position(pos)))

	describe("OrElse / AndThenWithCut") {
		it ("all failures reports the first part of each andThen chain, having not reached the cut yet") {
			val initialInput = new Input[Nothing](InputPart("1234", 42) :: Nil, Nil)
			val leftLeftParser = new ConstFailure(ExpectingDescription("LeftLeft"), Cut.False)
			val leftRightParser = new ConstFailure(ExpectingDescription("LeftRight"), Cut.False)
			val rightLeftParser = new ConstFailure(ExpectingDescription("RightLeft"), Cut.False)
			val rightRightParser = new ConstFailure(ExpectingDescription("RightRight"), Cut.False)

			val expected = Failure[Nothing](
				OrTrace(
					LeafTrace(leftLeftParser.expecting, initialInput),
					LeafTrace(rightLeftParser.expecting, initialInput)
				),
				Cut.False
			)
			val parser = (leftLeftParser andThenWithCut leftRightParser) orElse (rightLeftParser andThenWithCut rightRightParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("(Success ~/ Failure) | Failure ") {
			val initialInput = new Input[Nothing](InputPart("1234", 42) :: Nil, Nil)
			val leftLeftParser = new ConstSuccess(new Object, new Input[Nothing](InputPart("leftleft", 0) :: Nil, Nil), ExpectingDescription("LeftLeft"), Cut.False)
			val leftRightParser = new ConstFailure(ExpectingDescription("LeftRight"), Cut.False)
			val rightLeftParser = new ConstFailure(ExpectingDescription("RightLeft"), Cut.False)
			val rightRightParser = new ConstFailure(ExpectingDescription("RightRight"), Cut.False)

			val expected = Failure[Nothing](
				LeafTrace(leftRightParser.expecting, leftLeftParser.rest),
				Cut.True
			)
			val parser = (leftLeftParser andThenWithCut leftRightParser) orElse (rightLeftParser andThenWithCut rightRightParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("(Success ~/ Failure) | (Success ~/ Failure) ") {
			val initialInput = new Input[Nothing](InputPart("1234", 42) :: Nil, Nil)
			val leftLeftParser = new ConstSuccess(new Object, new Input[Nothing](InputPart("leftleft", 0) :: Nil, Nil), ExpectingDescription("LeftLeft"), Cut.False)
			val leftRightParser = new ConstFailure(ExpectingDescription("LeftRight"), Cut.False)
			val rightLeftParser = new ConstSuccess(new Object, new Input[Nothing](InputPart("rightleft", 0) :: Nil, Nil), ExpectingDescription("RightLeft"), Cut.False)
			val rightRightParser = new ConstFailure(ExpectingDescription("RightRight"), Cut.False)

			val expected = Failure[Nothing](
				LeafTrace(leftRightParser.expecting, leftLeftParser.rest),
				Cut.True
			)
			val parser = (leftLeftParser andThenWithCut leftRightParser) orElse (rightLeftParser andThenWithCut rightRightParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("(Success ~/ Success ~ Failure) | (Whatever)   is still cut ") {
			val initialInput = new Input[Nothing](InputPart("1234", 42) :: Nil, Nil)
			val leftLeftParser = new ConstSuccess(new Object, new Input[Nothing](InputPart("leftleft", 0) :: Nil, Nil), ExpectingDescription("LeftLeft"), Cut.False)
			val leftMiddleParser = new ConstSuccess(new Object, new Input[Nothing](InputPart("leftmiddle", 0) :: Nil, Nil), ExpectingDescription("LeftMiddle"), Cut.False)
			val leftRightParser = new ConstFailure(ExpectingDescription("LeftRight"), Cut.False)
			val rightParser = new ConstSuccess(new Object, new Input[Nothing](InputPart("rightleft", 0) :: Nil, Nil), ExpectingDescription("RightLeft"), Cut.False)

			val expected = Failure[Nothing](
				ThenTrace(
					ThenTrace(
						LeafTrace(leftLeftParser.expecting, initialInput),
						LeafTrace(leftMiddleParser.expecting, leftLeftParser.rest)
					),
					LeafTrace(leftRightParser.expecting, leftMiddleParser.rest)
				),
				Cut.True
			)
			val parser = (leftLeftParser andThenWithCut leftMiddleParser andThen leftRightParser) orElse (rightParser)
			assertResult(expected){parser.parse(initialInput)}
		}
	}
}
