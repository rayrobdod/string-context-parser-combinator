package com.rayrobdod.stringContextParserCombinator
package internal

import org.scalatest.funspec.AnyFunSpec
import com.rayrobdod.stringContextParserCombinator.RepeatStrategy._
import TestUtilities._

final class OptionallyTest extends AnyFunSpec {
	describe ("Optionally.interpolator") {
		it ("`a?` matches ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn[Id, Id]("a")

			val expected = Success[Nothing, StubPosition, Option[Char]](
				None,
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42)
			)
			val parser = childParser.optionally(strategy = Greedy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a?` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn[Id, Id]("a")

			val expected = Success[Nothing, StubPosition, Option[Char]](
				Success1(
					Some('a'),
					SinglePartInput("", 43),
					SingleExpecting("CharIn(\"a\")", 42)
				),
				List(
					Success1(
						None,
						SinglePartInput("a", 42),
						EmptyExpecting
					)
				)
			)
			val parser = childParser.optionally(strategy = Greedy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a?` does not match all of `aaaa`") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn[Id, Id]("a")

			val expected = Success[Nothing, StubPosition, Option[Char]](
				Success1(
					Some('a'),
					SinglePartInput("aaa", 43),
					SingleExpecting("CharIn(\"a\")", 42)
				),
				List(
					Success1(
						None,
						SinglePartInput("aaaa", 42),
						EmptyExpecting
					)
				)
			)
			val parser = childParser.optionally(strategy = Greedy)
			assertResult(expected){parser.interpolate(initialInput)}
		}

		it ("`a?+` matches ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn[Id, Id]("a")

			val expected = Success[Nothing, StubPosition, Option[Char]](
				None,
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42)
			)
			val parser = childParser.optionally(strategy = Possessive)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a?+` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn[Id, Id]("a")

			val expected = Success[Nothing, StubPosition, Option[Char]](
				Success1(
					Some('a'),
					SinglePartInput("", 43),
					SingleExpecting("CharIn(\"a\")", 42)
				),
				List(
				)
			)
			val parser = childParser.optionally(strategy = Possessive)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a?+` does not match all of `aaaa`") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn[Id, Id]("a")

			val expected = Success[Nothing, StubPosition, Option[Char]](
				Success1(
					Some('a'),
					SinglePartInput("aaa", 43),
					SingleExpecting("CharIn(\"a\")", 42)
				),
				List(
				)
			)
			val parser = childParser.optionally(strategy = Possessive)
			assertResult(expected){parser.interpolate(initialInput)}
		}

		it ("`a??` matches ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn[Id, Id]("a")

			val expected = Success[Nothing, StubPosition, Option[Char]](
				None,
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42)
			)
			val parser = childParser.optionally(strategy = Lazy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a??` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn[Id, Id]("a")

			val expected = Success[Nothing, StubPosition, Option[Char]](
				Success1(
					None,
					SinglePartInput("a", 42),
					EmptyExpecting
				),
				List(
					Success1(
						Some('a'),
						SinglePartInput("", 43),
						SingleExpecting("CharIn(\"a\")", 42)
					)
				)
			)
			val parser = childParser.optionally(strategy = Lazy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
		it ("`a??` does not match all of `aaaa`") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn[Id, Id]("a")

			val expected = Success[Nothing, StubPosition, Option[Char]](
				Success1(
					None,
					SinglePartInput("aaaa", 42),
					EmptyExpecting
				),
				List(
					Success1(
						Some('a'),
						SinglePartInput("aaa", 43),
						SingleExpecting("CharIn(\"a\")", 42)
					)
				)
			)
			val parser = childParser.optionally(strategy = Lazy)
			assertResult(expected){parser.interpolate(initialInput)}
		}
	}
}
