package com.rayrobdod.stringContextParserCombinator

import org.scalatest.funspec.AnyFunSpec

final class ParserTest extends AnyFunSpec {
	object Parsers extends com.rayrobdod.stringContextParserCombinator.Parsers {
		override type ContextType = Nothing
	}
	import Parsers.{CharIn, CodePointIn}

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
	describe("CodePointIn") {
		it ("Single success value") {
			val dut = CodePointIn("1")
			assertParseSuccessValue(CodePoint('1'))(dut, "1")
		}
		it ("Single failure message") {
			val exp = "Found EOF ; Expected \"1\""
			val dut = CodePointIn("1")
			assertParseFailureMessage(exp)(dut, "")
		}
		it ("Two failure message") {
			val exp = "Found EOF ; Expected \"1\" | \"2\""
			val dut = CodePointIn("12")
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
		it ("expected displays both halves") {
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
	describe("AndThen / Repeat chain") {
		it ("First") {
			val exp = "Found EOF ; Expected \"a\""
			val dut = CharIn("a") andThen CharIn("a").repeat()
			assertParseFailureMessage(exp)(dut, "")
		}
		it ("Second") {
			val dut = CharIn("a") andThen CharIn("a").repeat()
			assertParseSuccessValue(('a',""))(dut, "a")
		}
		it ("Fourth") {
			val dut = CharIn("a") andThen CharIn("a").repeat()
			assertParseSuccessValue(('a',"aa"))(dut, "aaa")
		}
	}
	describe("Repeat / AndThen chain") {
		it ("Zero-length input with any repeat") {
			val exp = "Found EOF ; Expected \"a\" | \"b\""
			val dut = CharIn("a").repeat() andThen CharIn("b")
			assertParseFailureMessage(exp)(dut, "")
		}
		it ("Missing lhs with any repeat") {
			val exp = "Found EOF ; Expected \"a\" | \"b\""
			val dut = CharIn("a").repeat() andThen CharIn("b")
			assertParseFailureMessage(exp)(dut, "a")
		}
		it ("Input too short for repeat") {
			val exp = "Found EOF ; Expected \"a\""
			val dut = CharIn("a").repeat(3, 5) andThen CharIn("b")
			assertParseFailureMessage(exp)(dut, "a")
		}
		it ("Input too long for repeat") {
			val exp = "Found \"aaa\" ; Expected \"b\""
			val dut = CharIn("a").repeat(3, 5) andThen CharIn("b")
			assertParseFailureMessage(exp)(dut, "aaaaaaaa")
		}
		it ("pattern.repeat andThen subset") {
			val dut = CharIn("ab").repeat() andThen CharIn("a")
			assertParseSuccessValue(("a",'a'))(dut, "aa")
		}
		it ("pattern.repeat andThen subset twice") {
			val dut = CharIn("ab").repeat() andThen CharIn("a") andThen CharIn("a")
			assertParseSuccessValue((("a",'a'),'a'))(dut, "aaa")
		}
	}
	describe("Repeat / OrElse chain") {
		it ("First") {
			val exp = "Found EOF ; Expected \"a\" | \"b\""
			val dut = CharIn("a").repeat(1) orElse CharIn("b")
			assertParseFailureMessage(exp)(dut, "")
		}
	}
	describe("Repeat / Repeat chain") {
		it ("Should not hang indefinitely") {
			val dut = CharIn("a").repeat().repeat()
			assertParseSuccessValue(List(""))(dut, "v")
		}
	}
}
