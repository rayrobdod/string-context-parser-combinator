package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec

final class OrElse_AndThen_Test extends AnyFunSpec {
	val positionOfNothing:Nothing => Position = x => x
	def SinglePartInput(str:String, pos:Int) = new Input[Nothing](((str, Position(pos))) :: Nil, Nil, positionOfNothing)
	def SimpleExpecting(msg:String) = Set(Expecting(msg, Position(0)))

	describe("OrElse / AndThen") {
		it ("all failures reports only the first part of each andThen chain") {
			val initialInput = SinglePartInput("1234", 42)
			val leftLeftParser = new ConstFailure(SimpleExpecting("LeftLeft"), Cut.False)
			val leftRightParser = new ConstFailure(SimpleExpecting("LeftRight"), Cut.False)
			val rightLeftParser = new ConstFailure(SimpleExpecting("RightLeft"), Cut.False)
			val rightRightParser = new ConstFailure(SimpleExpecting("RightRight"), Cut.False)

			val expected = Failure(
				leftLeftParser.expecting ++ rightLeftParser.expecting,
				Cut.False
			)
			val parser = (leftLeftParser andThen leftRightParser) orElse (rightLeftParser andThen rightRightParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("Success ~ Failure | Failure ") {
			val initialInput = SinglePartInput("1234", 42)
			val leftLeftParser = new ConstSuccess(new Object, SinglePartInput("leftleft", 0), SimpleExpecting("LeftLeft"), Cut.False)
			val leftRightParser = new ConstFailure(SimpleExpecting("LeftRight"), Cut.False)
			val rightLeftParser = new ConstFailure(SimpleExpecting("RightLeft"), Cut.False)
			val rightRightParser = new ConstFailure(SimpleExpecting("RightRight"), Cut.False)

			val expected = Failure(
				leftLeftParser.expecting ++ leftRightParser.expecting ++ rightLeftParser.expecting,
				Cut.False
			)
			val parser = (leftLeftParser andThen leftRightParser) orElse (rightLeftParser andThen rightRightParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("Success ~ Failure | Success ~ Failure ") {
			val initialInput = SinglePartInput("1234", 42)
			val leftLeftParser = new ConstSuccess(new Object, SinglePartInput("leftleft", 0), SimpleExpecting("LeftLeft"), Cut.False)
			val leftRightParser = new ConstFailure(SimpleExpecting("LeftRight"), Cut.False)
			val rightLeftParser = new ConstSuccess(new Object, SinglePartInput("rightleft", 0), SimpleExpecting("RightLeft"), Cut.False)
			val rightRightParser = new ConstFailure(SimpleExpecting("RightRight"), Cut.False)

			val expected = Failure(
				leftLeftParser.expecting ++ leftRightParser.expecting ++ rightLeftParser.expecting ++ rightRightParser.expecting,
				Cut.False
			)
			val parser = (leftLeftParser andThen leftRightParser) orElse (rightLeftParser andThen rightRightParser)
			assertResult(expected){parser.parse(initialInput)}
		}
	}
}
