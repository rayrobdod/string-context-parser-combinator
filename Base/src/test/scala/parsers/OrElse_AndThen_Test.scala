package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec
import TestUtilities._

final class OrElse_AndThen_Test extends AnyFunSpec {
	def SimpleExpecting(msg:String) = SingleExpecting(msg, 0)

	describe("OrElse / AndThen") {
		it ("all failures reports only the first part of each andThen chain") {
			val initialInput = SinglePartInput("1234", 42)
			val leftLeftParser = new ConstFailure(SimpleExpecting("LeftLeft"))
			val leftRightParser = new ConstFailure(SimpleExpecting("LeftRight"))
			val rightLeftParser = new ConstFailure(SimpleExpecting("RightLeft"))
			val rightRightParser = new ConstFailure(SimpleExpecting("RightRight"))

			val expected = Failure(
				leftLeftParser.expecting ++ rightLeftParser.expecting
			)
			val parser = (leftLeftParser andThen leftRightParser) orElse (rightLeftParser andThen rightRightParser)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("Success ~ Failure | Failure ") {
			val initialInput = SinglePartInput("1234", 42)
			val leftLeftParser = new ConstSuccess(new Object, SinglePartInput("leftleft", 0), SimpleExpecting("LeftLeft"))
			val leftRightParser = new ConstFailure(SimpleExpecting("LeftRight"))
			val rightLeftParser = new ConstFailure(SimpleExpecting("RightLeft"))
			val rightRightParser = new ConstFailure(SimpleExpecting("RightRight"))

			val expected = Failure(
				leftLeftParser.expecting ++ leftRightParser.expecting ++ rightLeftParser.expecting
			)
			val parser = (leftLeftParser andThen leftRightParser) orElse (rightLeftParser andThen rightRightParser)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("Success ~ Failure | Success ~ Failure ") {
			val initialInput = SinglePartInput("1234", 42)
			val leftLeftParser = new ConstSuccess(new Object, SinglePartInput("leftleft", 0), SimpleExpecting("LeftLeft"))
			val leftRightParser = new ConstFailure(SimpleExpecting("LeftRight"))
			val rightLeftParser = new ConstSuccess(new Object, SinglePartInput("rightleft", 0), SimpleExpecting("RightLeft"))
			val rightRightParser = new ConstFailure(SimpleExpecting("RightRight"))

			val expected = Failure(
				leftLeftParser.expecting ++ leftRightParser.expecting ++ rightLeftParser.expecting ++ rightRightParser.expecting
			)
			val parser = (leftLeftParser andThen leftRightParser) orElse (rightLeftParser andThen rightRightParser)
			assertResult(expected){parser.interpolate(initialInput)}
		}
	}
}
