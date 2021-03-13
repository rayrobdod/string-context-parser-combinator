package com.rayrobdod

import scala.Predef.refArrayOps
import scala.collection.immutable.Seq
import scala.language.higherKinds
import scala.reflect.api.Exprs
import scala.reflect.api.Universe
import scala.reflect.macros.Context

/**
 * A library for implementing StringContext methods via Parser Combinators
 *
 * @groupprio Parser 100
 * @groupprio macro 200
 * @groupprio Input/Result 300
 */
package object stringContextParserCombinator {
	private[this] val Name = new Extractor[Universe#Name, String] {
		def unapply(input:Universe#Name):Option[String] = Option(input.decodedName.toString)
	}

	private[this] def stringContextApply(c:Context):Extractor[c.Tree, List[c.Expr[String]]] = new Extractor[c.Tree, List[c.Expr[String]]] {
		import c.universe._ // ApplyTag, SelectTag etc.
		def unapply(tree:c.Tree):Option[List[c.Expr[String]]] = tree.duplicate match {
			case c.universe.Apply(
				c.universe.Select(
					c.universe.Select(
						c.universe.Ident(Name("scala")),
						Name("StringContext")
					),
					Name("apply")
				),
				strings
			) => Option(strings.map(x => c.Expr(x)))
			case _ => None
		}
	}

	private[this] def selectChain(c:Context, name:String):Extractor0[c.Tree] = new Extractor0[c.Tree] {
		import c.universe._ // ApplyTag, SelectTag etc.
		def unapply(tree:c.Tree):Boolean = {
			if (name.contains(".")) {
				val (nameInit, nameLast) = {
					val parts = name.split("\\.")
					(String.join(".", parts.init:_*), parts.last)
				}
				val NameInit = selectChain(c, nameInit)
				tree.duplicate match {
					// I want to write `case c.universe.Select(NameInit(), NameLast())`, and I
					// think I should be able to, but the compiler explodes whenever I attempt it
					case c.universe.Select(init, Name(`nameLast`)) if NameInit.unapply(init) => true
					case _ => false
				}
			} else {
				tree.duplicate match {
					case c.universe.Ident(Name(`name`)) => true
					case _ => false
				}
			}
		}
	}

	/**
	 * Returns a string representation of this input, suitable for printing to a users
	 */
	private[this] def inputDescription(input:Input[Exprs#Expr[_]]):String = {
		if (input.isEmpty) {
			"end of input"
		} else {
			scala.collection.immutable.Range(0, input.args.size)
				.map(i => s"${input.parts(i)._1}$${${input.args(i).tree}}")
				.mkString("\"", "", input.parts(input.args.size)._1 + "\"")
		}
	}

	/**
	 * Returns the position of this input
	 */
	private[this] def inputPosition(input:Input[Exprs#Expr[_]]):Position = {
		if (input.parts(0)._1.length != 0) {
			input.parts(0)._2
		} else if (input.args.nonEmpty) {
			Position(input.args(0).tree.pos)
		} else {
			input.parts(0)._2
		}
	}

	private[this] def reportFailure(c:Context)(failure:Failure[c.Expr[_]]):Nothing = {
		val trimmedTrace = failure.trace.removeRequiredThens.removeEmptyTraces
		val remainingDescription = inputDescription(trimmedTrace.leftMostRemaining)
		val remainingPosition = inputPosition(trimmedTrace.leftMostRemaining)
		val expectingDescription = trimmedTrace.expectingDescription

		remainingPosition.throwError(c)(s"Found ${remainingDescription} ; Expected ${expectingDescription}")
	}


	/**
	 * A macro impl scaffold, which takes care of extracting strings from a
	 * StringContext prefix, creating a parser with that value, then interpreting
	 * the parse result
	 *
	 * == Usage ==
	 *
	 * Given a StringContext extension class
	 * {{{
	 * package object \$package {
	 * 	implicit final class \$extensionclass(val backing:StringContext) extends AnyVal {
	 * 		def \$method(args:\$paramtype*):\$rettype = macro \$impl_method
	 * 	}
	 * }
	 * }}}
	 *
	 * Then, macro implementation should consist of
	 * {{{
	 * def \$impl_method(c:Context)(args:c.Expr[\$paramtype]*):c.Expr[\$rettype] = {
	 * 	val parser = ???
	 * 	macroimpl(c)("\$package.package.\$extensionclass", parser)(args)
	 * }
	 * }}}
	 *
	 * @group macro
	 */
	def macroimpl[Z](c:Context)(extensionClassName:String, parser:Parser[c.Expr[_], c.Expr[Z]])(args:Seq[c.Expr[Any]]):c.Expr[Z] = {
		val ExtensionClassSelectChain = selectChain(c, extensionClassName)
		val StringContextApply = stringContextApply(c)

		import c.universe._ // ApplyTag, SelectTag etc.
		val strings = c.prefix.tree.duplicate match {
			case c.universe.Apply(
				ExtensionClassSelectChain(),
				List(StringContextApply(strings))
			) => {
				strings.map({x => (c.eval(c.Expr[String](c.resetAllAttrs(x.tree.duplicate))), Position(x.tree.pos))})
			}
			case _ => c.abort(c.enclosingPosition, s"Do not know how to process this tree: " + c.universe.showRaw(c.prefix))
		}

		val input = new Input[c.Expr[Any]](strings, args.toList)

		parser.parse(input) match {
			case s:Success[_, _] => {
				//System.out.println(s.value)
				s.value
			}
			case f:Failure[_] => {
				reportFailure(c)(f)
			}
		}
	}
}

package stringContextParserCombinator {
	/** An object that can be a pattern-match pattern */
	private[stringContextParserCombinator] trait Extractor0[A] {def unapply(a:A):Boolean}
	/** An object that can be a pattern-match pattern */
	private[stringContextParserCombinator] trait Extractor[A,Z] {def unapply(a:A):Option[Z]}
	/** Support for Parsers.Lifted; represents a macro-level function that combines a CC[A] and an A. */
	trait LiftFunction[U <: Context with Singleton, CC[A], Z] {def apply[A](lifter:U#Expr[CC[A]], elem:U#Expr[A]):Z}



	/** A position in a source file */
	final case class Position(value:Int) extends AnyVal {
		def +(x:Int):Position = new Position(this.value + x)
		def throwError(c:Context)(msg:String):Nothing = c.abort(c.enclosingPosition.withPoint(value), msg)
	}
	object Position {
		def apply(x:scala.reflect.api.Position):Position = new Position(x.point)
	}
}
