package name.rayrobdod.stringContextParserCombinatorExample.aggregateLiteralTest

import name.rayrobdod.stringContextParserCombinatorExample.aggregateLiteral._

final class CaseClassTest extends munit.FunSuite {
	case class Point(x: Int, y: Int)
	case class Triangle(p1: Point, p2: Point, p3: Point)

	test("Simple") {
		val exp = Point(12, 23)
		val res: Point = cc"[12, 23]"
		assertEquals(res, exp)
	}
	test("Simple with names") {
		val exp = Point(34, 45)
		val res: Point = cc"[x = 34, y = 45]"
		assertEquals(res, exp)
	}
	test("Simple with names and interpolations") {
		val exp = Point(56, 67)
		val res: Point = cc"[x = ${exp.x}, y = ${exp.y}]"
		assertEquals(res, exp)
	}

	test("Nested case classes") {
		val exp = Triangle(Point(12, 23), Point(34,45), Point(56,67))
		val res: Triangle = cc"[[12,23], p2 = [x = 34, y = 45], ${exp.p3}]"
		assertEquals(res, exp)
	}
}
