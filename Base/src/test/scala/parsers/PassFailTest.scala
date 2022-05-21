package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec
import TestUtilities._

final class PassFailTest extends AnyFunSpec {
	describe ("Pass") {
		it ("returns a success") {
			val input = SinglePartInput("expect", 42)
			val expected = Success(
				(),
				input,
				EmptyExpecting,
				Cut.False
			)
			assertResult(expected)(Pass.parse(input))
		}
	}
	describe ("Fail") {
		it ("returns a failure") {
			val input = SinglePartInput("expect", 42)
			val description = "description"
			val expected = Failure(
				SingleExpecting(description, 42),
				Cut.False
			)
			assertResult(expected)(Fail(ExpectingDescription(description)).parse(input))
		}
	}
}
