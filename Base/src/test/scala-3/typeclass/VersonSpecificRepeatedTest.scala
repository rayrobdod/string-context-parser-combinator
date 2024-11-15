package name.rayrobdod.stringContextParserCombinator
package typeclass
package repeated

import scala.collection.mutable.Builder
import scala.quoted.*
import munit.Location

final class QuotedConcatenateStringTest extends munit.FunSuite {
	inline def assertParseSuccess(
		inline sc: StringContext,
		inline args: Any*)(
		expecting:String)(
		using loc:Location
	):Unit = ${
		QuotedConcatenateStringTestImpls.assertParseSuccessImpl(
			'this, 'sc, 'args, 'expecting, 'loc)
	}

	test ("0") {
		assertParseSuccess(StringContext(""))("")
	}
	test ("1") {
		assertParseSuccess(StringContext("ab"))("ab")
	}
	test ("many") {
		assertParseSuccess(StringContext("ab", "cd", "ef"), "12", "34")("ab12cd34ef")
	}
}

final class QuotedFromSplicesUsingBuilderTest extends munit.FunSuite {
	inline def assertParseSuccess[Z](
		newAcc: Builder[Int, Z],
		ifZeroCond: Boolean,
		ifZero: () => Z,
		ifOneCond: Boolean,
		ifOne: (Int) => Z,
		inline elems: (Int | Seq[Int]) *)(
		expecting: Z)(
		using loc:Location
	):Unit = ${
		QuotedFromSplicesUsingBuilderTestImpls.assertParseSuccessImpl(
			'this, 'newAcc, 'ifZeroCond, 'ifZero, 'ifOneCond, 'ifOne, 'elems, 'expecting, 'loc)
	}

	test ("0 items with ifZero") {
		assertParseSuccess(List.newBuilder, true, () => -1, true, x => x)(-1)
	}
	test ("0 items without ifZero") {
		assertParseSuccess(List.newBuilder, false, () => -1, false, x => x)(Nil)
	}
	test ("1 items with ifOne") {
		assertParseSuccess(List.newBuilder, true, () => -1, true, x => x, 2)(2)
	}
	test ("1 items without ifOne") {
		assertParseSuccess(List.newBuilder, false, () => -1, false, x => x, 2)(2 :: Nil)
	}
	test ("multiple individual items") {
		assertParseSuccess(List.newBuilder, false, () => -1, false, x => x, 1,2,3)(1 :: 2 :: 3 :: Nil)
	}
	test ("Splice items") {
		assertParseSuccess(List.newBuilder, true, () => -1, true, x => x, Seq(1,2))(1 :: 2 :: Nil)
	}
}
