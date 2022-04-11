package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.collection.immutable.Set
import org.scalatest.funspec.AnyFunSpec
import TestUtilities._

final class CharInTest extends AnyFunSpec {
	final case class Expr(value:String, pos:Int)
	def InputPart(str:String, pos:Int) = ((str, Position(pos)))
	val exprToPosition:Expr => Position = (expr:Expr) => Position(expr.pos)

	def expectSuccess(head:Char, restOfSet:Set[Char], tail:(List[(String, Position)], List[Expr]), expecting:Set[Expecting]) = {
		val input = new Input(((s"${head}${tail._1.head._1}", tail._1.head._2 + -1)) :: tail._1.tail, tail._2, exprToPosition)
		val parserSet = restOfSet + head
		val expected = Success(
			head,
			new Input(tail._1, tail._2, exprToPosition),
			expecting,
			Cut.False
		)
		val parser = CharIn[Expr](parserSet)
		assertResult(expected){parser.parse(input)}
	}

	def expectFailure(parserSet:Set[Char], input:Input[Expr]) = {
		val expected = Failure(
			Expecting(ExpectingDescription(parserSet.mkString("CharIn(\"", "", "\")")), input.position),
			Cut.False
		)
		val parser = CharIn[Expr](parserSet)
		assertResult(expected){parser.parse(input)}
	}

	describe("CharIn") {
		it ("Fails to parse an empty input") {
			expectFailure(Set('1', '2', '3'), new Input(InputPart("", 1) :: Nil, Nil, exprToPosition))
		}
		it ("Fails to parse when next value is an Arg") {
			expectFailure(Set('1', '2', '3'), new Input(InputPart("", 1) :: InputPart("More", 1) :: Nil, Expr("Arg", 101) :: Nil, exprToPosition))
		}
		it ("1 | 1") {
			expectSuccess(
				'1',
				Set.empty,
				(("", Position(1)) :: Nil, Nil),
				SingleExpecting("CharIn(\"1\")", 0)
			)
		}
		it ("123 | 1") {
			expectSuccess(
				'1',
				Set('2', '3'),
				(("", Position(1)) :: Nil, Nil),
				SingleExpecting("CharIn(\"231\")", 0)
			)
		}
		it ("1 | 123") {
			expectSuccess(
				'1',
				Set.empty,
				(("23", Position(2)) :: Nil, Nil),
				SingleExpecting("CharIn(\"1\")", 1)
			)
		}
	}
}
