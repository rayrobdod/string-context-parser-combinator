package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.collection.immutable.Set

object TestUtilities {
	val EmptyExpecting = ExpectingSet.empty[StubPosition]
	def SinglePartInput(str:String, pos:Int) = new Input[Nothing, StubPosition](((str, StubPosition(pos))) :: Nil, Nil)
	def SingleExpecting(msg:String, pos:Int) = ExpectingSet(Expecting(ExpectingDescription(msg), StubPosition(pos)))
	def RepeatedExpecting(msg:String, pos:Range) = ExpectingSet.fromSpecific(pos.map(x => Expecting(ExpectingDescription(msg), StubPosition(x))))


	implicit class ParserExtras[Expr, A](val self:Parser[Expr, A]) extends AnyVal {
		def map[Z](fn:Function1[A, Z]):Parser[Expr, Z] =
			new Map(self, fn)

		def flatMap[Z](fn:Function1[A, Parser[Expr, Z]]):Parser[Expr, Z] =
			new FlatMap(self, fn.andThen(new com.rayrobdod.stringContextParserCombinator.Parser(_)))

		def filter(predicate:Function1[A, Boolean], description:String):Parser[Expr, A] =
			new parsers.Filter(self, predicate, ExpectingDescription(description))

		def opaque(description:String):Parser[Expr, A] =
			new parsers.Opaque(self, ExpectingDescription(description))

		def andThen[ExprZ <: Expr, B, Z](rhs:Parser[ExprZ, B])(implicit ev:typeclass.Sequenced[A,B,Z]):Parser[ExprZ, Z] =
			new AndThen(self, rhs, ev)

		def andThenWithCut[ExprZ <: Expr, B, Z](rhs:Parser[ExprZ, B])(implicit ev:typeclass.Sequenced[A,B,Z]):Parser[ExprZ, Z] =
			new AndThenWithCut(self, rhs, ev)

		def orElse[ExprZ <: Expr, B, Z](rhs:Parser[ExprZ, B])(implicit ev:typeclass.Eithered[A,B,Z]):Parser[ExprZ, Z] =
			new OrElse(self, rhs, ev)

		def repeat[ExprZ <: Expr, Z](
			min:Int = 0,
			max:Int = Integer.MAX_VALUE,
			delimiter:Parser[ExprZ, Unit] = parsers.Pass,
			strategy:RepeatStrategy = RepeatStrategy.Possessive)(
			implicit ev:typeclass.Repeated[A, Z]
		):Parser[ExprZ, Z] = new Repeat(self, min, max, delimiter, strategy, ev)
	}
}
