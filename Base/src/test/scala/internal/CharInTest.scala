package com.rayrobdod.stringContextParserCombinator
package internal

import scala.collection.immutable.Set
import org.scalatest.funspec.AnyFunSpec
import TestUtilities._

object CharInTest {
	final case class Expr(value:String)
}

final class CharInTest extends AnyFunSpec {
	import CharInTest._
	def InputPart(str:String, pos:Int) = ((str, StubPosition(pos)))

	def expectSuccess(head:Char, restOfSet:Set[Char], tail:(List[(String, StubPosition)], List[(Expr, StubPosition)]), expecting:ExpectingSet[StubPosition]) = {
		val input = new Input(((s"${head}${tail._1.head._1}", tail._1.head._2 + -1)) :: tail._1.tail, tail._2)
		val parserSet = restOfSet + head
		val expected = Success(
			head,
			new Input(tail._1, tail._2),
			expecting
		)
		val parser = CharIn(parserSet)
		assertResult(expected){parser.interpolate(input)}
	}

	def expectFailure(parserSet:Set[Char], input:Input[Expr, StubPosition]) = {
		val expected = Failure(
			ExpectingSet(Expecting(ExpectingDescription(parserSet.mkString("CharIn(\"", "", "\")")), input.position))
		)
		val parser = CharIn(parserSet)
		assertResult(expected){parser.interpolate(input)}
	}

	describe("CharIn") {
		it ("Fails to interpolate an empty input") {
			expectFailure(Set('1', '2', '3'), new Input(InputPart("", 1) :: Nil, Nil))
		}
		it ("Fails to interpolate when next value is an Arg") {
			expectFailure(Set('1', '2', '3'), new Input(InputPart("", 1) :: InputPart("More", 1) :: Nil, (Expr("Arg"), StubPosition(101)) :: Nil))
		}
		it ("1 | 1") {
			expectSuccess(
				'1',
				Set.empty,
				(("", StubPosition(1)) :: Nil, Nil),
				SingleExpecting("CharIn(\"1\")", 0)
			)
		}
		it ("123 | 1") {
			expectSuccess(
				'1',
				Set('2', '3'),
				(("", StubPosition(1)) :: Nil, Nil),
				SingleExpecting("CharIn(\"231\")", 0)
			)
		}
		it ("1 | 123") {
			expectSuccess(
				'1',
				Set.empty,
				(("23", StubPosition(2)) :: Nil, Nil),
				SingleExpecting("CharIn(\"1\")", 1)
			)
		}
	}
}
