package com.rayrobdod.stringContextParserCombinator
package parsers

final class ConstSuccess[A](val a:A, val rest:Input[Nothing], val expecting:Expecting, val cut:Cut) extends Parser[Nothing, A] {
	def parse(input:Input[Nothing]):Result[Nothing, A] = {
		Success[Nothing, A](a, rest, LeafTrace(expecting, input), cut)
	}
}

final class ConstFailure(val expecting:Expecting, val cut:Cut) extends Parser[Nothing, Nothing] {
	def parse(input:Input[Nothing]):Result[Nothing, Nothing] = {
		Failure[Nothing](LeafTrace(expecting, input), cut)
	}
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
			.getOrElse(Failure(LeafTrace(Expecting("Known Input"), input), Cut.False))
	}
}

object Sequence {
	final case class Output[A](aOpt:Option[A], rest:Input[Nothing], expect:Expecting, cut:Cut) {
		def toResult(traceInput:Input[Nothing]):Result[Nothing, A] = aOpt match {
			case Some(a) => Success(a, rest, LeafTrace(expect, traceInput), cut)
			case None => Failure(LeafTrace(expect, traceInput), cut)
		}
	}
}
