package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.collection.immutable.Set
import org.scalatest.funspec.AnyFunSpec
import TestUtilities._

object CodepointInTest {
	final case class Expr(value:String, pos:Int)
}

final class CodepointInTest extends AnyFunSpec {
	import CodepointInTest._
	def InputPart(str:String, pos:Int) = ((str, StubPosition(pos)))
	val exprToPosition:Expr => StubPosition = (expr:Expr) => StubPosition(expr.pos)

	def expectSuccess(head:CodePoint, restOfSet:Set[CodePoint], tail:(List[(String, StubPosition)], List[Expr]), expecting:Set[Expecting[StubPosition]]) = {
		val input = new Input(((s"${head}${tail._1.head._1}", tail._1.head._2 + (if (head.value < 0x10000) {-1} else {-2}))) :: tail._1.tail, tail._2, exprToPosition)
		val parserSet = restOfSet + head
		val expected = Success(
			head,
			new Input(tail._1, tail._2, exprToPosition),
			expecting,
			Cut.False
		)
		val parser = CodePointIn[Expr](parserSet)
		assertResult(expected){parser.parse(input)}
	}

	def expectFailure(parserSet:Set[CodePoint], input:Input[Expr, StubPosition]) = {
		val expected = Failure(
			Set(Expecting(ExpectingDescription(parserSet.mkString("CodePointIn(\"", "", "\")")), input.position)),
			Cut.False
		)
		val parser = CodePointIn[Expr](parserSet)
		assertResult(expected){parser.parse(input)}
	}

	describe("CodepointIn") {
		it ("Fails to parse an empty input") {
			expectFailure(Set(CodePoint('1'), CodePoint('2'), CodePoint('3')), new Input(InputPart("", 1) :: Nil, Nil, exprToPosition))
		}
		it ("Fails to parse when next value is an Arg") {
			expectFailure(Set(CodePoint('1'), CodePoint('2'), CodePoint('3')), new Input(InputPart("", 1) :: InputPart("More", 1) :: Nil, Expr("Arg", 101) :: Nil, exprToPosition))
		}
		it ("1 | 1") {
			expectSuccess(
				CodePoint('1'),
				Set.empty,
				(("", StubPosition(1)) :: Nil, Nil),
				SingleExpecting("CodePointIn(\"1\")", 0)
			)
		}
		it ("123 | 1") {
			expectSuccess(
				CodePoint('1'),
				Set(CodePoint('2'), CodePoint('3')),
				(("", StubPosition(1)) :: Nil, Nil),
				SingleExpecting("CodePointIn(\"231\")", 0)
			)
		}
		it ("1 | 123") {
			expectSuccess(
				CodePoint('1'),
				Set.empty,
				(("23", StubPosition(2)) :: Nil, Nil),
				SingleExpecting("CodePointIn(\"1\")", 1)
			)
		}
		it ("Can match extended plane codepoints") {
			expectSuccess(
				CodePoint(0x1F342),
				Set.empty,
				(("", StubPosition(2)) :: Nil, Nil),
				SingleExpecting("CodePointIn(\"\uD83C\uDF42\")", 0)
			)
		}
	}
}
