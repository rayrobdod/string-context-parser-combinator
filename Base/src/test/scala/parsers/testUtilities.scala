package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.collection.immutable.Set

object TestUtilities {
	val EmptyExpecting = Set.empty[Expecting[StubPosition]]
	def SinglePartInput(str:String, pos:Int) = new Input[Nothing, StubPosition](((str, StubPosition(pos))) :: Nil, Nil, x => x)
	def SingleExpecting(msg:String, pos:Int) = Set(Expecting(ExpectingDescription(msg), StubPosition(pos)))
	def RepeatedExpecting(msg:String, pos:Range) = pos.flatMap(x => SingleExpecting(msg, x)).toSet


	implicit class ParserExtras[Expr, A](val self:Parser[Expr, A]) extends AnyVal {
		def map[Z](fn:Function1[A, Z]):Parser[Expr, Z] =
			new Map(self, fn)

		def flatMap[Z](fn:Function1[A, Parser[Expr, Z]]):Parser[Expr, Z] =
			new FlatMap(self, fn.andThen(new com.rayrobdod.stringContextParserCombinator.Parser(_)))

		def filter(predicate:Function1[A, Boolean], description:String):Parser[Expr, A] =
			new parsers.Filter(self, predicate, ExpectingDescription(description))

		def opaque(description:String):Parser[Expr, A] =
			new parsers.Opaque(self, ExpectingDescription(description))

		def andThen[ExprZ <: Expr, B, Z](rhs:Parser[ExprZ, B])(implicit ev:typelevel.Sequenced[A,B,Z]):Parser[ExprZ, Z] =
			new AndThen(self, rhs, ev)

		def andThenWithCut[ExprZ <: Expr, B, Z](rhs:Parser[ExprZ, B])(implicit ev:typelevel.Sequenced[A,B,Z]):Parser[ExprZ, Z] =
			new AndThenWithCut(self, rhs, ev)

		def orElse[ExprZ <: Expr, B, Z](rhs:Parser[ExprZ, B])(implicit ev:typelevel.Eithered[A,B,Z]):Parser[ExprZ, Z] =
			new OrElse(self, rhs, ev)

		def repeat[ExprZ <: Expr, Z](
			min:Int = 0,
			max:Int = Integer.MAX_VALUE,
			delimiter:Parser[ExprZ, Unit] = parsers.Pass,
			strategy:RepeatStrategy = RepeatStrategy.Possessive)(
			implicit ev:typelevel.Repeated[A, Z]
		):Parser[ExprZ, Z] = new Repeat(self, min, max, delimiter, strategy, ev)
	}
}
