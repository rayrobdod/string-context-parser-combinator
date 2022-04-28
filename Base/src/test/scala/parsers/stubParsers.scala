package com.rayrobdod.stringContextParserCombinator
package parsers

final class ConstSuccess[A](val a:A, val rest:Input[Nothing], val expecting:Set[Expecting], val cut:Cut) extends Parser[Nothing, A] {
	def parse(input:Input[Nothing]):Result[Nothing, A] = {
		Success[Nothing, A](a, rest, expecting, cut)
	}
}

final class ConstFailure(val expecting:Set[Expecting], val cut:Cut) extends Parser[Nothing, Nothing] {
	def parse(input:Input[Nothing]):Result[Nothing, Nothing] = {
		Failure(expecting, cut)
	}
}

final class ConstResult[Expr, A](val result:Result[Expr, A]) extends Parser[Expr, A] {
	def parse(input:Input[Expr]):Result[Expr, A] = result
}

final class Sequence[A](val initialInput:Input[Nothing], val outputs:Seq[Sequence.Output[A]]) extends Parser[Nothing, A] {
	val lookup = {
		val inputs = initialInput +: outputs.map{_.rest}
		inputs.zip(outputs).toMap
	}

	def parse(input:Input[Nothing]):Result[Nothing, A] = {
		lookup
			.get(input)
			.map(_.toResult(input))
			.getOrElse(Failure(Set(Expecting(ExpectingDescription("Known Input"), initialInput.position)), Cut.False))
	}
}

object Sequence {
	final case class Output[A](aOpt:Option[A], rest:Input[Nothing], expect:Set[Expecting], cut:Cut) {
		def toResult(traceInput:Input[Nothing]):Result[Nothing, A] = aOpt match {
			case Some(a) => Success(a, rest, expect, cut)
			case None => Failure(expect, cut)
		}
	}
}
