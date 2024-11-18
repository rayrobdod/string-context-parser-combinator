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
		ifZeroDefined: Boolean,
		ifZeroApply: () => Z,
		ifOneScalarDefined: Boolean,
		ifOneScalarApply: Int => Z,
		ifOneSpliceDefined: Boolean,
		ifOneSpliceApply: IterableOnce[Int] => Z,
		inline elems: (Int | Seq[Int]) *)(
		expecting: Z)(
		using loc:Location
	):Unit = ${
		QuotedFromSplicesUsingBuilderTestImpls.assertParseSuccessImpl(
			'this,
			'newAcc,
			'ifZeroDefined, 'ifZeroApply,
			'ifOneScalarDefined, 'ifOneScalarApply,
			'ifOneSpliceDefined, 'ifOneSpliceApply,
			'elems,
			'expecting,
			'loc
		)
	}

	test ("0 items with ifZero") {
		assertParseSuccess(List.newBuilder, true, () => -1, true, x => x, true, x => x)(-1)
	}
	test ("0 items without ifZero") {
		assertParseSuccess(List.newBuilder, false, () => -1, false, x => x, false, x => x)(Nil)
	}
	test ("1 scalar with ifOneScalar") {
		assertParseSuccess(List.newBuilder, true, () => -1, true, x => x, true, x => x, 2)(2)
	}
	test ("1 scalar without ifOneScalar") {
		assertParseSuccess(List.newBuilder, false, () => -1, false, x => x, false, x => x, 2)(2 :: Nil)
	}
	test ("multiple scalars") {
		assertParseSuccess(List.newBuilder, true, () => -1, true, x => x, true, x => x, 1,2,3)(1 :: 2 :: 3 :: Nil)
	}
	test ("1 splice with ifOneSplice") {
		assertParseSuccess(List.newBuilder, true, () => -1, true, x => x, true, x => x.iterator.mkString, Seq(1,2))("12")
	}
	test ("1 splice without ifOneSplice") {
		assertParseSuccess(List.newBuilder, false, () => -1, false, x => x, false, x => x.iterator.mkString, Seq(1,2))(1 :: 2 :: Nil)
	}
	test ("multiple splices") {
		assertParseSuccess(List.newBuilder, true, () => -1, true, x => x, true, x => x, Seq(1,2), Seq(3,4))(1 :: 2 :: 3 :: 4 :: Nil)
	}
	test ("mix") {
		assertParseSuccess(List.newBuilder, true, () => -1, true, x => x, true, x => x, 42, Seq(1,2), 151)(42 :: 1 :: 2 :: 151 :: Nil)
	}
}
