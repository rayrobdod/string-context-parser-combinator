package com.rayrobdod

import scala.Predef.refArrayOps
import scala.collection.immutable.Seq
import scala.language.higherKinds
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import scala.quoted.Varargs

/**
 * A library for implementing StringContext methods via Interpolator Combinators
 */
package object stringContextParserCombinator {
	private[stringContextParserCombinator] def reportFailure(failure:Failure[Position.Impl])(using Quotes):Nothing = {
		failure.expecting match {
			case ExpectingSet.Empty() => {
				scala.quoted.quotes.reflect.report.errorAndAbort("Parsing failed")
			}
			case ExpectingSet.NonEmpty(position, descriptions) => {
				// `sorted` to make result deterministic
				val descriptions2 = descriptions.toList.sortBy(_.toString).mkString("Expected ", " or ", "")
				position.errorAndAbort(descriptions2)
			}
		}
	}
}

package stringContextParserCombinator {
	/** Support for [[Interpolator.Lifted]]; represents a macro-level function that combines a CC[A] and an A. */
	trait LiftFunction[-CC[_], +Z] {def apply[A](lifter:Expr[CC[A]], elem:Expr[A])(using Type[A], Quotes):Z}
	/** An identity context - for parsing outside of a macro */
	type Id[+A] = A
	/** An identity function for lifting into the identity context */
	type IdToExpr[A] = =:=[A, A]


	/*
	 * All this complexity with Position is so that the unit tests don't have to find a
	 * scala.quoted.Quotes or blackbox.Context in order to check how the position has
	 * advanced after a parser has run
	 */
	/** Represents a position in a source file. Indicates where to point to in compile error messages */
	private[stringContextParserCombinator]
	trait Position[Pos] {
		extension (pos:Pos) def +(offset:Int):Pos = this.offset(pos, offset)
		def offset(pos:Pos, offset:Int):Pos
	}

	private[stringContextParserCombinator]
	object Position {
		/** The canonical production-use Position type */
		final class Impl(private[Position] val q:Quotes)(private[Position] val file:q.reflect.SourceFile, private[Position] val start:Int, private[Position] val end:Int) {
			def errorAndAbort(msg:String):Nothing = {
				q.reflect.report.errorAndAbort(msg, q.reflect.Position(file, start, end))
			}
			override def toString:String = s"Position.Impl($file, $start, $end)"
			override def hashCode:Int = this.start * 31 + this.end
			override def equals(other:Any):Boolean = other match {
				case x:Impl => this.file == x.file && this.start == x.start && this.end == x.end
				case _ => false
			}

		}

		object Impl {
			// Probably can assume that any positions compared will have the same sourceFile
			given Ordering[Impl] = Ordering.by(_.start)
			given Position[Impl] = (pos:Impl, offset:Int) => new Impl(pos.q)(pos.file, pos.start + offset, pos.end)
		}

		def apply(expr:Expr[_])(using q:Quotes):Impl = {
			import q.reflect._
			val pos = expr.asTerm.pos
			new Impl(q)(pos.sourceFile, pos.start, pos.end)
		}
	}
}
