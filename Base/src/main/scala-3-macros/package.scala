package com.rayrobdod

import scala.Predef.refArrayOps
import scala.collection.immutable.Seq
import scala.language.higherKinds
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import scala.quoted.Varargs

/**
 * A library for implementing StringContext methods via Parser Combinators
 *
 * @groupprio Parser 100
 * @groupprio macro 200
 * @groupprio Input/Result 300
 */
package object stringContextParserCombinator {
	/**
	 * Returns a string representation of this input, suitable for printing to a users
	 */
	private[this] def inputDescription(input:Input[Expr[_]])(using c:Quotes):String = {
		if (input.isEmpty) {
			"end of input"
		} else {
			scala.collection.immutable.Range(0, input.args.size)
				.map(i => s"${input.parts(i)._1}$${${input.args(i).show}}")
				.mkString("\"", "", input.parts(input.args.size)._1 + "\"")
		}
	}

	/**
	 * Returns the position of this input
	 */
	private[this] def inputPosition(input:Input[Expr[_]])(using Quotes):Position = {
		if (input.parts(0)._1.length != 0) {
			input.parts(0)._2
		} else if (input.args.nonEmpty) {
			Position(input.args(0))
		} else {
			input.parts(0)._2
		}
	}

	private[this] def reportFailure(failure:Failure[Expr[_]])(using c:Quotes):Nothing = {
		val trimmedTrace = failure.trace.removeRequiredThens.removeEmptyTraces
		val remainingDescription = inputDescription(trimmedTrace.leftMostRemaining)
		val remainingPosition = inputPosition(trimmedTrace.leftMostRemaining)
		val expectingDescription = trimmedTrace.expectingDescription

		remainingPosition.throwError(s"Found ${remainingDescription} ; Expected ${expectingDescription}")
	}

	/**
	 * A macro impl scaffold, which takes care of extracting strings from a
	 * StringContext prefix, creating a parser with that value, then interpreting
	 * the parse result
	 *
	 * == Usage ==
	 *
	 * Given a StringContext extension
	 * {{{
	 * package object \$package {
	 * 	extension (inline sc:scala.StringContext)
	 * 		inline def \$method(inline args:\$paramtype*):\$rettype = macro \$impl_method
	 * 	}
	 * }
	 * }}}
	 *
	 * Then, macro implementation should consist of
	 * {{{
	 * def \$impl_method(sc:Expr[scala.StringContext], args:c.Expr[\$paramtype]*)(using Quotes):Expr[\$rettype] = {
	 * 	val parser:Parser[Expr[\$rettype]] = ???
	 * 	macroimpl(parser)(sc, args)
	 * }
	 * }}}
	 *
	 * @group macro
	 */
	def macroimpl[Z](parser:Parser[Expr[_], Expr[Z]])(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using c:Quotes):Expr[Z] = {
		val strings = sc match {
			case '{ _root_.scala.StringContext(${Varargs(args)}: _*) } => args
			case _ => scala.quoted.quotes.reflect.report.throwError(s"Do not know how to process this tree", sc)
		}
		val strings2 = strings.map(x => ((x.valueOrError, Position(x)))).toList
		val args2 = Varargs.unapply(args).get.toList

		val input = new Input(strings2, args2)

		parser.parse(input) match {
			case Success(res, _, _, _) => {
				//System.out.println(res.show)
				res
			}
			case f@Failure(_, _) => {
				reportFailure(f)
			}
		}
	}
}

package stringContextParserCombinator {
	/** Support for Parsers.Lifted; represents a macro-level function that combines a CC[A] and an A. */
	trait LiftFunction[CC[A], Z] {def apply[A](lifter:Expr[CC[A]], elem:Expr[A])(using Quotes, Type[A]):Expr[Z]}

	trait TypeFunction[Lifter[A]]{def apply[A](t:Type[A])(using Quotes):Type[Lifter[A]]}


	/** A position in a source file */
	trait Position {
		def +(rhs:Int):Position
		def throwError(msg:String):Nothing
	}
	object Position {
		final class Impl(q:Quotes)(file:q.reflect.SourceFile, start:Int, end:Int) extends Position {
			def +(rhs:Int):Position = new Impl(q)(file, start + rhs, end)
			def throwError(msg:String):Nothing = {
				q.reflect.report.throwError(msg, q.reflect.Position(file, start, end))
			}
		}

		def apply(expr:Expr[_])(using q:Quotes):Position = {
			import q.reflect._
			stringContextParserCombinator.Position(expr.asTerm.pos)
		}

		def apply(using q:Quotes)(pos:q.reflect.Position):Position = {
			Position(using q)(pos.sourceFile, pos.start, pos.end)
		}

		def apply(using q:Quotes)(file:q.reflect.SourceFile, start:Int, end:Int):Position = {
			new Impl(q)(file, start, end)
		}

		private[stringContextParserCombinator] def apply(point:Int):Position = {
			final case class Impl2(point:Int) extends Position {
				def +(rhs:Int):Position = new Impl2(point + rhs)
				def throwError(msg:String):Nothing = {
					throw new NotImplementedError
				}
			}
			new Impl2(point)
		}
	}
}
