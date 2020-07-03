package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec

final class OrElse_AndThen_Test extends AnyFunSpec {
	def InputPart(str:String, pos:Int) = ((str, PositionPoint(pos)))

	describe("OrElse / AndThen") {
		it ("all failures reports only the first part of each andThen chain") {
			val initialInput = new Input[Nothing](InputPart("1234", 42) :: Nil, Nil)
			val leftLeftParser = new ConstFailure(Expecting("LeftLeft"), Cut.False)
			val leftRightParser = new ConstFailure(Expecting("LeftRight"), Cut.False)
			val rightLeftParser = new ConstFailure(Expecting("RightLeft"), Cut.False)
			val rightRightParser = new ConstFailure(Expecting("RightRight"), Cut.False)

			val expected = Failure[Nothing](
				OrTrace(
					LeafTrace(leftLeftParser.expecting, initialInput),
					LeafTrace(rightLeftParser.expecting, initialInput)
				),
				Cut.False
			)
			val parser = (leftLeftParser andThen leftRightParser) orElse (rightLeftParser andThen rightRightParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("Success ~ Failure | Failure ") {
			val initialInput = new Input[Nothing](InputPart("1234", 42) :: Nil, Nil)
			val leftLeftParser = new ConstSuccess(new Object, new Input[Nothing](InputPart("leftleft", 0) :: Nil, Nil), Expecting("LeftLeft"), Cut.False)
			val leftRightParser = new ConstFailure(Expecting("LeftRight"), Cut.False)
			val rightLeftParser = new ConstFailure(Expecting("RightLeft"), Cut.False)
			val rightRightParser = new ConstFailure(Expecting("RightRight"), Cut.False)

			val expected = Failure[Nothing](
				OrTrace(
					ThenTrace(
						LeafTrace(leftLeftParser.expecting, initialInput),
						LeafTrace(leftRightParser.expecting, leftLeftParser.rest)
					),
					LeafTrace(rightLeftParser.expecting, initialInput)
				),
				Cut.False
			)
			val parser = (leftLeftParser andThen leftRightParser) orElse (rightLeftParser andThen rightRightParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("Success ~ Failure | Success ~ Failure ") {
			val initialInput = new Input[Nothing](InputPart("1234", 42) :: Nil, Nil)
			val leftLeftParser = new ConstSuccess(new Object, new Input[Nothing](InputPart("leftleft", 0) :: Nil, Nil), Expecting("LeftLeft"), Cut.False)
			val leftRightParser = new ConstFailure(Expecting("LeftRight"), Cut.False)
			val rightLeftParser = new ConstSuccess(new Object, new Input[Nothing](InputPart("rightleft", 0) :: Nil, Nil), Expecting("RightLeft"), Cut.False)
			val rightRightParser = new ConstFailure(Expecting("RightRight"), Cut.False)

			val expected = Failure[Nothing](
				OrTrace(
					ThenTrace(
						LeafTrace(leftLeftParser.expecting, initialInput),
						LeafTrace(leftRightParser.expecting, leftLeftParser.rest)
					),
					ThenTrace(
						LeafTrace(rightLeftParser.expecting, initialInput),
						LeafTrace(rightRightParser.expecting, rightLeftParser.rest)
					)
				),
				Cut.False
			)
			val parser = (leftLeftParser andThen leftRightParser) orElse (rightLeftParser andThen rightRightParser)
			assertResult(expected){parser.parse(initialInput)}
		}
	}
}
