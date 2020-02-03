package com.rayrobdod.stringContextParserCombinator

import org.scalatest.funspec.AnyFunSpec

final class ParserTest extends AnyFunSpec {
	def CharIn(x:String):Parser[Nothing, Char] = new CharIn(scala.Predef.wrapString(x))

	def assertParseFailureMessage(expected:String)(dut:Parser[Nothing, _], inputStr:String):Unit = {
		val input = Input[Nothing](List((inputStr, PositionPoint(0))), List())
		dut.parse(input) match {
			case _:Success[_,_] => fail("Parse Succeeded")
			case f:Failure => assertResult(expected)(f.msg)
		}
	}
	def assertParseSuccessValue(expected:Any)(dut:Parser[Nothing, _], inputStr:String):Unit = {
		val input = Input[Nothing](List((inputStr, PositionPoint(0))), List())
		dut.parse(input) match {
			case s:Success[_,_] => assertResult(expected)(s.value)
			case _:Failure => fail("Parse Failed")
		}
	}

	describe("CharIn") {
		it ("Single success value") {
			val dut = CharIn("1")
			assertParseSuccessValue('1')(dut, "1")
		}
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
		it ("Success") {
			val exp = ('1', '2')
			val dut = CharIn("1") andThen CharIn("2")
			assertParseSuccessValue(exp)(dut, "12")
		}
		it ("Failure on first subparser") {
			val exp = "Found EOF ; Expected \"1\""
			val dut = CharIn("1") andThen CharIn("2")
			assertParseFailureMessage(exp)(dut, "")
		}
		it ("Failure on second subparser") {
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
		ignore ("pattern.repeat andThen subset") {
			val dut = CharIn("ab").repeat() andThen CharIn("a")
			assertParseSuccessValue(("a",'a'))(dut, "aa")
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
