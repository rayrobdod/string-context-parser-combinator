package com.rayrobdod.stringContextParserCombinator
package internal

import scala.collection.immutable.Set

object TestUtilities {
	val EmptyExpecting = ExpectingSet.empty[StubPosition]
	def SinglePartInput(str:String, pos:Int) = new Input[Nothing, StubPosition](((str, StubPosition(pos))) :: Nil, Nil)
	def SingleExpecting(msg:String, pos:Int) = ExpectingSet(Expecting(ExpectingDescription(msg), StubPosition(pos)))
	def RepeatedExpecting(msg:String, pos:Range) = ExpectingSet.fromSpecific(pos.map(x => Expecting(ExpectingDescription(msg), StubPosition(x))))


	implicit class ParserExtras[Expr, A](val self:Interpolator[Expr, A]) extends AnyVal {
		def map[Z](fn:Function1[A, Z]):Interpolator[Expr, Z] =
			internal.Map.interpolator(self, fn)

		def flatMap[Z](fn:Function1[A, Interpolator[Expr, Z]]):Interpolator[Expr, Z] =
			internal.FlatMap.interpolator(self, fn.andThen(new com.rayrobdod.stringContextParserCombinator.Interpolator(_)))

		def filter(predicate:Function1[A, Boolean], description:String):Interpolator[Expr, A] =
			internal.Filter.interpolator(self, predicate, ExpectingDescription(description))

		def opaque(description:String):Interpolator[Expr, A] =
			internal.Opaque.interpolator(self, ExpectingDescription(description))

		def andThen[ExprZ <: Expr, B, Z](rhs:Interpolator[ExprZ, B])(implicit ev:typeclass.Sequenced[A,B,Z]):Interpolator[ExprZ, Z] =
			internal.AndThen.interpolator(self, rhs, ev)

		def orElse[ExprZ <: Expr, B, Z](rhs:Interpolator[ExprZ, B])(implicit ev:typeclass.Eithered[A,B,Z]):Interpolator[ExprZ, Z] =
			internal.OrElse.interpolator(self, rhs, ev)

		def repeat[ExprZ <: Expr, Z](
			min:Int = 0,
			max:Int = Integer.MAX_VALUE,
			delimiter:Interpolator[ExprZ, Unit] = new internal.Pass[Id, Id],
			strategy:RepeatStrategy = RepeatStrategy.Possessive)(
			implicit ev:typeclass.Repeated[A, Z]
		):Interpolator[ExprZ, Z] = internal.Repeat.interpolator(self, min, max, delimiter, strategy, ev)

		def optionally[Z](
			strategy:RepeatStrategy = RepeatStrategy.Possessive)(
			implicit ev:typeclass.Optionally[A, Z]
		):Interpolator[Expr, Z] = internal.Optionally.interpolator(self, strategy, ev)
	}
}
