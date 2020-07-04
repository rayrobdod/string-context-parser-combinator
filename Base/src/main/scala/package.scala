package com.rayrobdod

import scala.Predef.refArrayOps
import scala.collection.immutable.Seq
import scala.language.higherKinds
import scala.reflect.api.Universe
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

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
				strings.map({x => (MacroCompat.eval(c)(x), PositionPoint(x.tree.pos))})
			}
			case _ => c.abort(c.enclosingPosition, s"Do not know how to process this tree: " + c.universe.showRaw(c.prefix))
		}

		val input = new Input[c.Expr[Any]](strings, args.toList)

		parser.parse(input) match {
			case s:Success[_, _] => {
				//System.out.println(res)
				s.value
			}
			case f:Failure[_] => {
				f.report(c)
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
	trait LiftFunction[U <: Context with Singleton, CC[A], Z] {def apply[A](lifter:U#Expr[CC[A]], elem:U#Expr[A]):U#Expr[Z]}


	// CodePoint extending AnyVal, parameterized methods, and using CodePoint::toString results in a
	// surprisingly high number of NoSuchMethodErrors, at least before 2.12.
	// `java.lang.NoSuchMethodError: 'java.lang.String com.rayrobdod.stringContextParserCombinator.CodePoint$.toString$extension(int)`
	/** A unicode codepoint */
	final case class CodePoint(val value:Int) {
		override def toString:String = new String(Array[Int](value), 0, 1)
	}

	/** A position's point - divorced from the position's context */
	final case class PositionPoint(val value:Int) extends AnyVal {
		def cast(c:Context):c.Position = c.enclosingPosition.withPoint(value)
		def +(x:Int):PositionPoint = PositionPoint(this.value + x)
		def >(rhs:PositionPoint):Boolean = this.value > rhs.value
	}
	object PositionPoint {
		def apply(x:scala.reflect.api.Position):PositionPoint = new PositionPoint(x.point)
	}

	/** Represent a textual description of under what conditions a parser would return success */
	final case class Expecting(val description:String)
}
