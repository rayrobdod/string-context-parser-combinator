package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.collection.immutable.Set
import org.scalatest.funspec.AnyFunSpec

final class CodepointInTest extends AnyFunSpec {
	final case class Expr(value:String)
	def InputPart(str:String, pos:Int) = ((str, PositionPoint(pos)))

	def expectSuccess(head:CodePoint, restOfSet:Set[CodePoint], tail:(List[(String, PositionPoint)], List[Expr])) = {
		val input = new Input(((s"${head}${tail._1.head._1}", tail._1.head._2 + (if (head.value < 0x10000) {-1} else {-2}))) :: tail._1.tail, tail._2)
		val parserSet = restOfSet + head
		val expected = Success(
			head,
			new Input(tail._1, tail._2),
			LeafTrace(Expecting(parserSet.mkString("CodePointIn(\"", "", "\")")), input)
		)
		val parser = CodePointIn[Expr](parserSet)
		assertResult(expected){parser.parse(input)}
	}

	def expectFailure(parserSet:Set[CodePoint], input:Input[Expr]) = {
		val expected = Failure(
			LeafTrace(Expecting(parserSet.mkString("CodePointIn(\"", "", "\")")), input)
		)
		val parser = CodePointIn[Expr](parserSet)
		assertResult(expected){parser.parse(input)}
	}

	describe("CodepointIn") {
		it ("Fails to parse an empty input") {
			expectFailure(Set(CodePoint('1'), CodePoint('2'), CodePoint('3')), new Input(InputPart("", 1) :: Nil, Nil))
		}
		it ("Fails to parse when next value is an Arg") {
			expectFailure(Set(CodePoint('1'), CodePoint('2'), CodePoint('3')), new Input(InputPart("", 1) :: InputPart("More", 1) :: Nil, Expr("Arg") :: Nil))
		}
		it ("1 | 1") {
			expectSuccess(CodePoint('1'), Set.empty, (("", PositionPoint(1)) :: Nil, Nil))
		}
		it ("123 | 1") {
			expectSuccess(CodePoint('1'), Set(CodePoint('2'), CodePoint('3')), (("", PositionPoint(1)) :: Nil, Nil))
		}
		it ("1 | 123") {
			expectSuccess(CodePoint('1'), Set.empty, (("23", PositionPoint(2)) :: Nil, Nil))
		}
		it ("Can match extended plane codepoints") {
			expectSuccess(CodePoint(0x1F342), Set.empty, (("", PositionPoint(1)) :: Nil, Nil))
		}
	}
}
