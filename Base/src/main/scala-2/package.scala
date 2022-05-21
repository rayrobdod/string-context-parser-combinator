package com.rayrobdod

import scala.Predef.refArrayOps
import scala.collection.immutable.Seq
import scala.reflect.api.Universe
import scala.reflect.macros.blackbox.Context

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

	private[stringContextParserCombinator]
	implicit class PositionSyntax[Pos](pos:Pos)(implicit ev:Position[Pos]) {
		def +(offset:Int):Pos = ev.offset(pos, offset)
	}

	private[this] def reportFailure(c:Context)(failure:Failure[Position.Impl]):Nothing = {
		val remainingPosition = failure.expecting.map(_.position).max
		val expectingDescription = failure.expecting.filter(_.position == remainingPosition).map(_.description).mkString(" or ")

		remainingPosition.errorAndAbort(c)(s"Expected ${expectingDescription}")
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
				strings.map({x => (c.eval(x), Position(x.tree.pos))})
			}
			case _ => c.abort(c.enclosingPosition, s"Do not know how to process this tree: " + c.universe.showRaw(c.prefix))
		}

		val input = new Input[c.Expr[Any], Position.Impl](strings, args.toList, x => Position(x.tree.pos))

		parser.parse(input) match {
			case s:Success[_, _, _] => {
				//System.out.println(s.value)
				s.choicesHead.value
			}
			case f:Failure[Position.Impl] => {
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



	/*
	 * All this complexity with Position is so that the unit tests don't have to find a
	 * scala.quoted.Quotes or blackbox.Context in order to execute parsers
	 */
	/** Represents a position in a source file. Indicates where to point to in compile error messages */
	private[stringContextParserCombinator]
	trait Position[Pos] {
		def offset(pos:Pos, offset:Int):Pos
	}
	private[stringContextParserCombinator]
	object Position {
		def apply(x:scala.reflect.api.Position):Position.Impl = new Position.Impl(x.point)

		/** The canonical production-use Position type */
		final class Impl(private[Position] val point:Int) {
			def errorAndAbort(c:Context)(msg:String):Nothing = c.abort(c.enclosingPosition.withPoint(point), msg)
			override def toString:String = s"Position.Impl($point)"
			override def hashCode:Int = point
			override def equals(other:Any):Boolean = other match {
				case x:Impl => this.point == x.point
				case _ => false
			}
		}

		object Impl {
			implicit val given_PositionImpl_Ordering:Ordering[Impl] = Ordering.by(_.point)
			implicit val given_PositionImpl_Position:Position[Impl] = new Position[Impl] {
				def offset(pos:Impl, offset:Int):Impl = new Impl(pos.point + offset)
			}
		}
	}
}
