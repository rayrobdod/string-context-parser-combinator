package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.collection.immutable.Set

final class ConstSuccess[A](val a:A, val rest:Input[Nothing, StubPosition], val expecting:Set[Expecting[StubPosition]], val cut:Cut) extends Parser[Nothing, A] {
	def parse[Pos](input:Input[Nothing, Pos]):Result[Nothing, Pos, A] = {
		Success[Nothing, StubPosition, A](a, rest, expecting, cut).asInstanceOf[Result[Nothing, Pos, A]]
	}
}

final class ConstFailure(val expecting:Set[Expecting[StubPosition]], val cut:Cut) extends Parser[Nothing, Nothing] {
	def parse[Pos](input:Input[Nothing, Pos]):Result[Nothing, Pos, Nothing] = {
		Failure(expecting, cut).asInstanceOf[Result[Nothing, Pos, Nothing]]
	}
}

final class ConstResult[Expr, A](val result:Result[Expr, StubPosition, A]) extends Parser[Expr, A] {
	def parse[Pos](input:Input[Expr, Pos]):Result[Expr, Pos, A] = result.asInstanceOf[Result[Expr, Pos, A]]
}

final class Sequence[A](val initialInput:Input[Nothing, StubPosition], val outputs:Seq[Sequence.Output[A]]) extends Parser[Nothing, A] {
	val lookup = {
		val inputs = initialInput +: outputs.map{_.rest}
		inputs.zip(outputs).toMap
	}

	def parse[Pos](input:Input[Nothing, Pos]):Result[Nothing, Pos, A] = {
		lookup
			.get(input.asInstanceOf[Input[Nothing, StubPosition]])
			.map(_.toResult(input.asInstanceOf[Input[Nothing, StubPosition]]).asInstanceOf[Result[Nothing, Pos, A]])
			.getOrElse(Failure(Set(Expecting(ExpectingDescription("Known Input"), initialInput.position.asInstanceOf[Pos])), Cut.False))
	}
}

object Sequence {
	final case class Output[A](aOpt:Option[A], rest:Input[Nothing, StubPosition], expect:Set[Expecting[StubPosition]], cut:Cut) {
		def toResult(traceInput:Input[Nothing, StubPosition]):Result[Nothing, StubPosition, A] = aOpt match {
			case Some(a) => Success(a, rest, expect, cut)
			case None => Failure(expect, cut)
		}
	}
}
