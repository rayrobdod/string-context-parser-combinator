package name.rayrobdod

import scala.reflect.api.Universe
import scala.reflect.macros.blackbox.Context
import scala.Predef._

/**
 * A library for implementing custom string interpolation implementations using Parser Combinators
 */
package object stringContextParserCombinator {
	/** An identity context - for parsing outside of a macro */
	type Id[+A] = A
	/** An identity function for lifting into the identity context */
	type IdToExpr[A] = =:=[A, A]

	private[stringContextParserCombinator]
	val Name = new Unapply.Fixed[Universe#Name, String] {
		def unapply(input:Universe#Name):Option[String] = Option(input.decodedName.toString)
	}

	private[stringContextParserCombinator]
	def stringContextApply(c:Context):Unapply.Fixed[c.Tree, List[c.Expr[String]]] = new Unapply.Fixed[c.Tree, List[c.Expr[String]]] {
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

	private[stringContextParserCombinator]
	def selectChain(c:Context, name:String):Unapply.Zero[c.Tree] = new Unapply.Zero[c.Tree] {
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

	private[stringContextParserCombinator]
	def reportFailure(c:Context)(failure:Failure[c.universe.Position]):Nothing = {
		failure.expecting match {
			case ExpectingSet.Empty() => {
				c.abort(c.enclosingPosition, "Parsing failed")
			}
			case set @ ExpectingSet.NonEmpty(position, _) => {
				c.abort(position, set.renderDescriptions)
			}
		}
	}
}

package stringContextParserCombinator {
	/** Support for Interpolator.contextInterpolators.lifted; represents a macro-level function that combines a CC[A] and an A. */
	trait LiftFunction[U <: Context with Singleton, -CC[_], +Z] {def apply[A](lifter:U#Expr[CC[A]], elem:U#Expr[A]):Z}
}
