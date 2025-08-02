package name.rayrobdod.stringContextParserCombinatorExample.quasiquotesTests

final class InterpolationTest extends munit.FunSuite {
	test("spliced expr") {
		inline def splicedValue[A](value: A) = ${ InterpolationMacros.splicedValueImpl('value) }
		val expected = new Object
		val actual = splicedValue(expected)
		assertEquals(actual, expected)
	}
	test("lifted string") {
		inline def actual = ${ InterpolationMacros.liftedStringImpl("Hello World") }
		val expected = "Hello World"
		assertEquals(actual, expected)
	}
	test("`null`") {
		inline def actual = ${ InterpolationMacros.nullImpl }
		val expected = null
		assertEquals(actual, expected)
	}
	test("`true`") {
		inline def actual = ${ InterpolationMacros.trueImpl }
		val expected = true
		assertEquals(actual, expected)
	}
	test("`false`") {
		inline def actual = ${ InterpolationMacros.falseImpl }
		val expected = false
		assertEquals(actual, expected)
	}
	test("`0`") {
		inline def actual = ${ InterpolationMacros.zeroImpl }
		val expected = 0
		assertEquals(actual, expected)
	}
	test("`1`") {
		inline def actual = ${ InterpolationMacros.oneImpl }
		val expected = 1
		assertEquals(actual, expected)
	}
	test("`-1`") {
		inline def actual = ${ InterpolationMacros.negOneImpl }
		val expected = -1
		assertEquals(actual, expected)
	}
	test("binary number with underscores") {
		inline def actual = ${ InterpolationMacros.binIntImpl }
		val expected = 172 // 0b1010_1100
		assertEquals(actual, expected)
	}
	test("decimal number with underscores") {
		inline def actual = ${ InterpolationMacros.decIntImpl }
		val expected = 0_1__2_3
		assertEquals(actual, expected)
	}
	test("hex number with underscores") {
		inline def actual = ${ InterpolationMacros.hexIntImpl }
		val expected = 0x12_34_56
		assertEquals(actual, expected)
	}
	test("hex long number") {
		inline def actual = ${ InterpolationMacros.hexLongImpl }
		val expected = 0x1234_5678_9ABC_DEF0L
		assertEquals(actual, expected)
	}
	test("prefix op ! on Boolean") {
		inline def actual = ${ InterpolationMacros.notTrueImpl }
		val expected = ! true
		assertEquals(actual, expected)
	}
	test("toFloat call on Int") {
		inline def actual = ${ InterpolationMacros.intToFloat }
		val expected = 25.toFloat
		assertEquals(actual, expected)
	}
	test("tail.tail.tail call on Range") {
		inline def dut(value: Range) = ${ InterpolationMacros.rangeTripleTail('value) }
		val base = 0 to 25 by 3
		val expected = base.tail.tail.tail
		val actual = dut(base)
		assertEquals(actual, expected)
	}
}
