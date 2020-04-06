package com.rayrobdod

import scala.collection.immutable.Seq
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/**
 * A library for implementing StringContext methods via Parser Combinators
 *
 * @groupprio Parser 100
 * @groupprio macro 200
 * @groupprio Input/Result 300
 */
package object stringContextParserCombinator {
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
	def macroimpl[Z](c:Context)(extensionClassName:String, parser:Parser[c.type, c.Expr[Z]])(args:Seq[c.Expr[Any]]):c.Expr[Z] = {
		val ExtensionClassSelectChain = Utilities.selectChain(c, extensionClassName)
		val StringContextApply = Utilities.stringContextApply(c)

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

		val input = new Input[c.type](strings, args.toList)

		parser.parse(input) match {
			case Success(res, _) => {
				//System.out.println(res)
				res
			}
			case f:Failure => {
				f.report(c)
			}
		}
	}


}

package stringContextParserCombinator {
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
}
