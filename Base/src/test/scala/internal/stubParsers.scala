package com.rayrobdod.stringContextParserCombinator
package internal

import scala.collection.immutable.Set

final class ConstSuccess[A](val a:A, val rest:Input[Nothing, StubPosition], val expecting:ExpectingSet[StubPosition]) extends Interpolator[Any, A] {
	def interpolate[ExprZ <: Any, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
		Success[ExprZ, StubPosition, A](a, rest, expecting).asInstanceOf[Result[ExprZ, Pos, A]]
	}
}

final class ConstFailure(val expecting:ExpectingSet[StubPosition]) extends Interpolator[Any, Nothing] {
	def interpolate[ExprZ <: Any, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Nothing] = {
		Failure(expecting).asInstanceOf[Result[ExprZ, Pos, Nothing]]
	}
}

final class ConstResult[Expr, A](val result:Result[Expr, StubPosition, A]) extends Interpolator[Expr, A] {
	def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = result.asInstanceOf[Result[ExprZ, Pos, A]]
}
