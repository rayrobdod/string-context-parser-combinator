package name.rayrobdod.stringContextParserCombinator
package typeclass
package repeated

import scala.collection.mutable.Builder
import scala.quoted.*
import munit.Location
import Interpolator.{charWhere, ofType, end}
import Repeated.SplicePiece

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

object QuotedFromSplicesUsingBuilderTestImpls {
	def assertParseSuccessImpl[Int, Z](
		self: Expr[munit.FunSuite],
		newAcc: Expr[Builder[Int,Z]],
		ifZeroCondExpr: Expr[Boolean],
		ifZero: Expr[() => Z],
		ifOneCondExpr: Expr[Boolean],
		ifOne: Expr[(Int) => Z],
		elems: Expr[Seq[Int | Seq[Int]]],
		expecting: Expr[Z],
		loc: Expr[Location])(
		using Quotes, Type[Int], Type[Z]
	):Expr[Unit] = {
		val Varargs(elems2) = elems: @unchecked
		val elems3: Seq[SplicePiece[Expr, Int]] = elems2.map:
			case '{ $x: Int } => SplicePiece.One[Expr, Int](x)
			case '{ $xs: Seq[Int] } => SplicePiece.Many[Expr, Int](xs)

		val ifZeroCond = ifZeroCondExpr.valueOrAbort
		val ifOneCond = ifOneCondExpr.valueOrAbort

		val ifZero2 = Option.when(ifZeroCond)({() => '{${ifZero}()}})
		val ifOne2 = Option.when(ifOneCond)({(a:Expr[Int]) => '{${ifOne}(${a})}})

		val dut = Repeated.quotedFromSplicesUsingBuilder(newAcc, ifZero2, ifOne2)

		val actual = dut.result(elems3.foldLeft(dut.init())((acc, elem) => dut.append(acc, elem)))

		'{
			given Location = ${loc}
			$self.assertEquals($actual, $expecting)
		}
	}
}
