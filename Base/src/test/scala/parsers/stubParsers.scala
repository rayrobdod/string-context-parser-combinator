package com.rayrobdod.stringContextParserCombinator
package parsers

final class ConstSuccess[A](val a:A, val rest:Input[Nothing], val expecting:Expecting) extends Parser[Nothing, A] {
	def parse(input:Input[Nothing]):Result[Nothing, A] = {
		Success[Nothing, A](a, rest, LeafTrace(expecting, input))
	}
}

final class ConstFailure(val expecting:Expecting) extends Parser[Nothing, Nothing] {
	def parse(input:Input[Nothing]):Result[Nothing, Nothing] = {
		Failure[Nothing](LeafTrace(expecting, input))
	}
}
