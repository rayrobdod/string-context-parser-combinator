package name.rayrobdod.stringContextParserCombinator
package typeclass
package repeated

import scala.quoted.*
import munit.Location
import Interpolator.{charWhere, ofType, end}

object QuotedConcatenateStringTestImpls {
	def assertParseSuccessImpl(
		self: Expr[munit.FunSuite],
		sc: Expr[StringContext],
		args: Expr[Seq[Any]],
		expecting: Expr[String],
		loc: Expr[Location])(
		using Quotes
	):Expr[Unit] = {

		val dut = (charWhere(_ => true).repeat(1).mapToExpr orElse ofType[String])
			.repeat()(using Repeated.quotedConcatenateString)
			.andThen(end)

		val actual = dut.interpolate(sc, args)

		'{
			given Location = ${loc}
			$self.assertEquals($actual, $expecting)
		}
	}
}
