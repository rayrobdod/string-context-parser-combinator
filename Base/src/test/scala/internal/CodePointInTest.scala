package com.rayrobdod.stringContextParserCombinator
package internal

import scala.collection.immutable.Set
import org.scalatest.funspec.AnyFunSpec
import TestUtilities._

object CodepointInTest {
	final case class Expr(value:String)
}

final class CodepointInTest extends AnyFunSpec {
	import CodepointInTest._
	def InputPart(str:String, pos:Int) = ((str, StubPosition(pos)))

	def expectSuccess(head:CodePoint, restOfSet:Set[CodePoint], tail:(List[(String, StubPosition)], List[(Expr, StubPosition)]), expecting:ExpectingSet[StubPosition]) = {
		val input = new Input((s"${head}${tail._1.head._1}", tail._1.head._2 + -head.charCount) :: tail._1.tail, tail._2)
		val parserSet = restOfSet + head
		val expected = Success(
			head,
			new Input(tail._1, tail._2),
			expecting
		)
		val parser = CodePointIn[Id, Id](parserSet)
		assertResult(expected){parser.interpolate(input)}
	}

	def expectFailure(parserSet:Set[CodePoint], input:Input[Expr, StubPosition]) = {
		val expected = Failure(
			ExpectingSet(Expecting(ExpectingDescription(parserSet.mkString("CodePointIn(\"", "", "\")")), input.position))
		)
		val parser = CodePointIn[Id, Id](parserSet)
		assertResult(expected){parser.interpolate(input)}
	}

	describe("CodepointIn") {
		it ("Fails to interpolate an empty input") {
			expectFailure(Set(CodePoint('1'), CodePoint('2'), CodePoint('3')), new Input(InputPart("", 1) :: Nil, Nil))
		}
		it ("Fails to interpolate when next value is an Arg") {
			expectFailure(Set(CodePoint('1'), CodePoint('2'), CodePoint('3')), new Input(InputPart("", 1) :: InputPart("More", 1) :: Nil, (Expr("Arg"), StubPosition(101)) :: Nil))
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