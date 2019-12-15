package com.rayrobdod.stringContextParserCombinator

import scala.reflect.macros.blackbox.Context

/**
 * The result of a parse
 * @group Result
 */
sealed trait Result[U <: Context with Singleton, +A] {
	private[stringContextParserCombinator] def map[Z](fn:Function1[A, Z]):Result[U, Z] = this match {
		case Success(v, r) => Success(fn(v), r)
		case Failure(ex) => Failure(ex)
	}
	private[stringContextParserCombinator] def orElse[Z >: A](other: => Result[U, Z]):Result[U, Z] = this match {
		case Success(v, r) => Success(v, r)
		case Failure(expected1) => other match {
			case Success(v, r) => Success(v, r)
			case Failure(expected2) => Failure(expected1 ++: expected2)
		}
	}
}

/**
 * @group Result
 */
final case class Success[U <: Context with Singleton, +A](
	val value:A,
	val remaining:Data[U]
) extends Result[U, A]

/**
 * @group Result
 */
final case class Failure[U <: Context with Singleton](
	val expecting:Seq[String]
) extends Result[U, Nothing]
