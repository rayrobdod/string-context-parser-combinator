package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.collection.immutable.Set
import org.scalatest.funspec.AnyFunSpec

final class CodepointInTest extends AnyFunSpec {
	final case class Expr(value:String, pos:Int)
	def InputPart(str:String, pos:Int) = ((str, Position(pos)))
	val exprToPosition:Expr => Position = (expr:Expr) => Position(expr.pos)

	def expectSuccess(head:CodePoint, restOfSet:Set[CodePoint], tail:(List[(String, Position)], List[Expr])) = {
		val input = new Input(((s"${head}${tail._1.head._1}", tail._1.head._2 + (if (head.value < 0x10000) {-1} else {-2}))) :: tail._1.tail, tail._2, exprToPosition)
		val parserSet = restOfSet + head
		val expected = Success(
			head,
			new Input(tail._1, tail._2, exprToPosition),
			Set.empty,
			Cut.False
		)
		val parser = CodePointIn[Expr](parserSet)
		assertResult(expected){parser.parse(input)}
	}

	def expectFailure(parserSet:Set[CodePoint], input:Input[Expr]) = {
		val expected = Failure(
			Expecting(parserSet.mkString("CodePointIn(\"", "", "\")"), input.position),
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
			expectSuccess(CodePoint('1'), Set.empty, (("", Position(1)) :: Nil, Nil))
		}
		it ("123 | 1") {
			expectSuccess(CodePoint('1'), Set(CodePoint('2'), CodePoint('3')), (("", Position(1)) :: Nil, Nil))
		}
		it ("1 | 123") {
			expectSuccess(CodePoint('1'), Set.empty, (("23", Position(2)) :: Nil, Nil))
		}
		it ("Can match extended plane codepoints") {
			expectSuccess(CodePoint(0x1F342), Set.empty, (("", Position(1)) :: Nil, Nil))
		}
	}
}
