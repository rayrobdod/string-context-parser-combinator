package com.rayrobdod.stringContextParserCombinator

import org.scalatest.FunSpec

final class ParserTest extends FunSpec {
	def CharIn(x:String):Parser[Nothing, Char] = new CharIn(scala.Predef.wrapString(x))

	def assertParseFailureMessage(expected:String)(dut:Parser[Nothing, _], inputStr:String):Unit = {
		val input = Input[Nothing](List((inputStr, PositionPoint(0))), List())
		dut.parse(input) match {
			case _:Success[_,_] => fail("Parse Succeeded")
			case f:Failure[_] => assertResult(expected)(f.msg)
		}
	}

	describe("CharIn") {
		it ("Single failure message") {
			val exp = "Found EOF ; Expected \"1\""
			val dut = CharIn("1")
			assertParseFailureMessage(exp)(dut, "")
		}
		it ("Two failure message") {
			val exp = "Found EOF ; Expected \"1\" | \"2\""
			val dut = CharIn("12")
			assertParseFailureMessage(exp)(dut, "")
		}
	}
	describe("AndThen") {
		it ("First") {
			val exp = "Found EOF ; Expected \"1\""
			val dut = CharIn("1") andThen CharIn("2")
			assertParseFailureMessage(exp)(dut, "")
		}
		it ("Second") {
			val exp = "Found EOF ; Expected \"2\""
			val dut = CharIn("1") andThen CharIn("2")
			assertParseFailureMessage(exp)(dut, "1")
		}
	}
	describe("OrElse") {
		it ("asdf") {
			val exp = "Found EOF ; Expected \"1\" | \"2\""
			val dut = CharIn("1") orElse CharIn("2")
			assertParseFailureMessage(exp)(dut, "")
		}
	}
	describe("OrElse / AndThen chain") {
		it ("First") {
			val exp = "Found EOF ; Expected \"1\" | \"2\""
			val dut = CharIn("1") orElse (CharIn("2") andThen CharIn("3"))
			assertParseFailureMessage(exp)(dut, "")
		}
		it ("Second") {
			val exp = "Found EOF ; Expected \"3\""
			val dut = CharIn("1") orElse (CharIn("2") andThen CharIn("3"))
			assertParseFailureMessage(exp)(dut, "2")
		}
	}
	describe("Repeat / AndThen chain") {
		ignore ("First") {
			val exp = "Found EOF ; Expected \"a\" | \"b\""
			val dut = CharIn("a").repeat() andThen CharIn("b")
			assertParseFailureMessage(exp)(dut, "")
		}
		ignore ("Second") {
			val exp = "Found EOF ; Expected \"a\" | \"b\""
			val dut = CharIn("a").repeat() andThen CharIn("b")
			assertParseFailureMessage(exp)(dut, "a")
		}
	}
	describe("Repeat / OrElse chain") {
		it ("First") {
			val exp = "Found EOF ; Expected \"a\" | \"b\""
			val dut = CharIn("a").repeat(1) orElse CharIn("b")
			assertParseFailureMessage(exp)(dut, "")
		}
	}
}
