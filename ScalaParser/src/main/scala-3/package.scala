package name.rayrobdod.stringContextParserCombinatorExample

import scala.quoted.*

package object quasiquotes:
	extension (inline sc: scala.StringContext)
		transparent inline def q(inline args: Any*)(using quotes: Quotes): Expr[?] =
			${ExpressionParsers.main('sc, 'args, 'quotes)}
