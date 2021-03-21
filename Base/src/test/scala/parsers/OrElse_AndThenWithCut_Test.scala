package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec
import TestUtilities._

final class OrElse_AndThenWithCut_Test extends AnyFunSpec {
	def SimpleExpecting(msg:String) = SingleExpecting(msg, 0)

	describe("OrElse / AndThenWithCut") {
		it ("all failures reports the first part of each andThen chain, having not reached the cut yet") {
			val initialInput = SinglePartInput("1234", 42)
			val leftLeftParser = new ConstFailure(SimpleExpecting("LeftLeft"), Cut.False)
			val leftRightParser = new ConstFailure(SimpleExpecting("LeftRight"), Cut.False)
			val rightLeftParser = new ConstFailure(SimpleExpecting("RightLeft"), Cut.False)
			val rightRightParser = new ConstFailure(SimpleExpecting("RightRight"), Cut.False)

			val expected = Failure(
				leftLeftParser.expecting ++ rightLeftParser.expecting,
				Cut.False
			)
			val parser = (leftLeftParser andThenWithCut leftRightParser) orElse (rightLeftParser andThenWithCut rightRightParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("(Success ~/ Failure) | Failure ") {
			val initialInput = SinglePartInput("1234", 42)
			val leftLeftParser = new ConstSuccess(new Object, SinglePartInput("leftleft", 0), SimpleExpecting("LeftLeft"), Cut.False)
			val leftRightParser = new ConstFailure(SimpleExpecting("LeftRight"), Cut.False)
			val rightLeftParser = new ConstFailure(SimpleExpecting("RightLeft"), Cut.False)
			val rightRightParser = new ConstFailure(SimpleExpecting("RightRight"), Cut.False)

			val expected = Failure(
				leftRightParser.expecting,
				Cut.True
			)
			val parser = (leftLeftParser andThenWithCut leftRightParser) orElse (rightLeftParser andThenWithCut rightRightParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("(Success ~/ Failure) | (Success ~/ Failure) ") {
			val initialInput = SinglePartInput("1234", 42)
			val leftLeftParser = new ConstSuccess(new Object, SinglePartInput("leftleft", 0), SimpleExpecting("LeftLeft"), Cut.False)
			val leftRightParser = new ConstFailure(SimpleExpecting("LeftRight"), Cut.False)
			val rightLeftParser = new ConstSuccess(new Object, SinglePartInput("rightleft", 0), SimpleExpecting("RightLeft"), Cut.False)
			val rightRightParser = new ConstFailure(SimpleExpecting("RightRight"), Cut.False)

			val expected = Failure(
				leftRightParser.expecting,
				Cut.True
			)
			val parser = (leftLeftParser andThenWithCut leftRightParser) orElse (rightLeftParser andThenWithCut rightRightParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("(Success ~/ Success ~ Failure) | (Whatever)   is still cut ") {
			val initialInput = SinglePartInput("1234", 42)
			val leftLeftParser = new ConstSuccess(new Object, SinglePartInput("leftleft", 0), Set.empty, Cut.False)
			val leftMiddleParser = new ConstSuccess(new Object, SinglePartInput("leftmiddle", 0), Set.empty, Cut.False)
			val leftRightParser = new ConstFailure(SimpleExpecting("LeftRight"), Cut.False)
			val rightParser = new ConstSuccess(new Object, SinglePartInput("rightleft", 0), SimpleExpecting("RightLeft"), Cut.False)

			val expected = Failure(
				leftRightParser.expecting,
				Cut.True
			)
			val parser = (leftLeftParser andThenWithCut leftMiddleParser andThen leftRightParser) orElse (rightParser)
			assertResult(expected){parser.parse(initialInput)}
		}

		it ("(Success ~/ (Failure | Failure)") {
			val initialInput = SinglePartInput("1234", 42)
			val leftParser = new ConstSuccess(new Object, SinglePartInput("left", 0), SimpleExpecting("Left"), Cut.False)
			val rightLeftParser = new ConstFailure(SimpleExpecting("RightLeft"), Cut.False)
			val rightRightParser = new ConstFailure(SimpleExpecting("RightRight"), Cut.False)

			val expected = Failure(
				rightLeftParser.expecting ++ rightRightParser.expecting,
				Cut.True
			)
			val parser = (leftParser) andThenWithCut (rightLeftParser orElse rightRightParser)
			assertResult(expected){parser.parse(initialInput)}
		}
	}
}
