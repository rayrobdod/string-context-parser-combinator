package com.rayrobdod.stringContextParserCombinator

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/**
 * The result of a parse
 * @group Result
 */
sealed trait Result[U <: Context with Singleton, +A] {
	private[stringContextParserCombinator] def map[Z](fn:Function1[A, Z]):Result[U, Z] = this match {
		case Success(v, r) => Success(fn(v), r)
		case Failure(found, expect) => Failure(found, expect)
	}
	private[stringContextParserCombinator] def orElse[Z >: A](other: => Result[U, Z]):Result[U, Z] = this match {
		case Success(v, r) => Success(v, r)
		case Failure(found1, expect1) => other match {
			case Success(v, r) => Success(v, r)
			case Failure(found2, expect2) => {
				if (found1._2 == found2._2) {Failure(found1, Failure.Or(Seq(expect1, expect2)))}
				else if (found1._2 > found2._2) {Failure(found1, expect1)}
				else {Failure(found2, expect2)}
			}
		}
	}
}

/**
 * @group Result
 */
final case class Success[U <: Context with Singleton, +A](
	val value:A,
	val remaining:Input[U]
) extends Result[U, A]

/**
 * @group Result
 */
final case class Failure[U <: Context with Singleton](
	val found:(String, PositionPoint),
	val expecting:Failure.Expecting
) extends Result[U, Nothing] {
	def msg:String =  s"Found ${found._1} ; Expected $expecting"
	def report(c:U):Nothing = {
		c.abort(found._2.cast(c), msg)
	}
}

object Failure {
	sealed trait Expecting
	final case class Leaf(x:String) extends Expecting {
		override def toString:String = x
	}
	final case class Or(options:Seq[Expecting]) extends Expecting {
		override def toString:String = options.mkString(" | ")
	}
}
