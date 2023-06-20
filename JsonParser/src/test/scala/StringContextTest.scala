package com.rayrobdod.stringContextParserCombinatorExample.jsonTest

import scala.collection.immutable.{Map, Vector}
import org.json4s.{JValue, JNull, JBool, JNumber, JLong, JInt, JDecimal, JString, JArray, JObject}
import com.rayrobdod.stringContextParserCombinatorExample.json._

final class StringContextTest extends munit.FunSuite {
	test("Accepts an immediate null") {
		val exp = JNull
		assertEquals(json"null", exp)
	}
	test("Accepts an immediate true") {
		val exp = JBool.True
		assertEquals(json"true", exp)
	}
	test("Accepts an immediate false") {
		val exp = JBool.False
		assertEquals(json"false", exp)
	}
	test("Accepts a scala.Boolean false") {
		val foo = false
		val exp = JBool.False
		assertEquals(json"$foo", exp)
	}
	test("Accepts a scalajson.ast.Boolean false") {
		val exp = JBool.False
		assertEquals(json"$exp", exp)
	}

	test("Accepts a literal 0") {
		val exp = JDecimal(0)
		assertEquals(json"0", exp)
	}
	test("Rejects a number with a leading zero") {
		assertNotEquals(
			compileErrors("""(json"012")"""),
			""
		)
	}
	test("Accepts a literal 1") {
		val exp = JDecimal(1)
		assertEquals(json"1", exp)
	}
	test("Accepts a literal neg1") {
		val exp = JDecimal(-1)
		assertEquals(json"-1", exp)
	}
	test("Accepts a literal 1.5") {
		val exp = JDecimal(1.5)
		assertEquals(json"1.5", exp)
	}
	test("Accepts a literal 1e2") {
		val exp = JDecimal(1e2)
		assertEquals(json"1e2", exp)
	}
	test("Accepts a literal 1e-2") {
		val exp = JDecimal(1e-2)
		assertEquals(json"1e-2", exp)
	}
	test("Accepts a literal 1.5e+2") {
		val exp = JDecimal(15e1)
		assertEquals(json"1.5e+2", exp)
	}
	test("Accepts a JNumber 1.5") {
		val exp:JValue with JNumber = JDecimal(1.5)
		assertEquals(json"$exp", exp)
	}
	test("Accepts an Integer 100") {
		val param = 100
		val exp = JLong(100)
		assertEquals(json"$param", exp)
	}
	test("Accepts an BigDecimal 4.20") {
		val param = BigDecimal("4.20")
		val exp = JDecimal(BigDecimal("4.20"))
		assertEquals(json"$param", exp)
	}
	test("Accepts a custom lift number") {
		object Pi {}
		implicit val liftPi:Lift[Pi.type, JValue with JNumber] = Lift(_ => JDecimal(BigDecimal("3.14")))
		val exp = JDecimal(BigDecimal("3.14"))
		assertEquals(json"$Pi", exp)
	}

	test("Accepts a String string") {
		val param = "abcd"
		val exp = JString("abcd")
		assertEquals(json"$param", exp)
	}
	test("Accepts a JString string") {
		val exp = JString("abcd")
		assertEquals(json"$exp", exp)
	}
	test("Accepts a custom lift string") {
		object Pi {}
		implicit val liftPi:Lift[Pi.type, JString] = Lift(_ => JString("π"))
		val exp = JString("π")
		assertEquals(json"$Pi", exp)
	}
	test("Accepts a literal string") {
		val exp = JString("abcd")
		assertEquals(json""""abcd"""", exp)
	}
	test("Accepts a literal empty string") {
		val exp = JString("")
		assertEquals(json""" "" """, exp)
	}
	test("Accepts a String embedded in a string") {
		val inner = "123"
		val exp = JString("ab123cd")
		assertEquals(json""""ab${inner}cd"""", exp)
	}
	test("Accepts a JString embedded in a string") {
		val inner = JString("123")
		val exp = JString("ab123cd")
		assertEquals(json""""ab${inner}cd"""", exp)
	}
	test("Accepts a custom lift embedded in a string") {
		object Pi {}
		implicit val liftPi:Lift[Pi.type, JString] = Lift(_ => JString("π"))
		val exp = JString("1π2")
		assertEquals(json""" "1${Pi}2" """, exp)
	}
	test("Accepts a literal with the simple escape sequences") {
		val exp = JString("\\/\"\n\r\b\f\t")
		assertEquals(json""" "\\\/\"\n\r\b\f\t" """, exp)
	}
	test("Accepts a literal escaped-unicode string") {
		val exp = JString("\u1234 \u09AF")
		assertEquals(json""" "\u1234 \u09AF" """, exp)
	}
	test("Does not take an excessively long time to match a longish string") {
		val exp = JString("12345678901234567890")
		assertEquals(json""" "12345678901234567890" """, exp)
	}

	test("Accepts a provided Vector") {
		val arg = List(JBool.True, JBool.False)
		val exp = JArray(arg)
		assertEquals(json"$arg", exp)
	}
	test("Accepts a provided Range") {
		import scala.Predef.intWrapper
		val exp = JArray(List(JLong(1), JLong(2), JLong(3), JLong(4)))
		assertEquals(json"${1 to 4}", exp)
	}
	test("Accepts an literal empty array") {
		val exp = JArray(List.empty)
		assertEquals(json"[]", exp)
	}
	test("Accepts a literal array with a single immediate value") {
		val exp = JArray(List(JDecimal(123)))
		assertEquals(json" [ 123 ] ", exp)
	}
	test("Accepts an array with two different immediate values") {
		val exp = JArray(List(JDecimal(123), JString("abc")))
		assertEquals(json""" [ 123 , "abc" ] """, exp)
	}
	test("Accepts an array with two variable values") {
		val arg1 = JLong(123)
		val arg2 = JString("abc")
		val exp = JArray(List(arg1, arg2))
		assertEquals(json""" [ $arg1 , $arg2 ] """, exp)
	}
	test("Vector inside a literal vector is kept as vector") {
		val arg = List(JLong(2), JLong(3))
		val exp = JArray(List(JDecimal(1), JArray(arg), JDecimal(4)))
		assertEquals(json""" [ 1 , $arg , 4 ] """, exp)
	}
	test("Splicing a JArray flattens the values") {
		val arg = JArray(List(JLong(2), JLong(3)))
		val exp = JArray(List(JDecimal(1), JLong(2), JLong(3), JDecimal(4)))
		assertEquals(json""" [ 1 , ..$arg , 4 ] """, exp)
	}
	test("Splicing a vector of jvalues flattens the values") {
		val arg = Vector(JLong(2), JLong(3))
		val exp = JArray(List(JDecimal(1), JLong(2), JLong(3), JDecimal(4)))
		assertEquals(json""" [ 1 , ..$arg , 4 ] """, exp)
	}
	test("Splicing a vector of ints flattens the values") {
		val arg = Vector(2, 3)
		val exp = JArray(List(JDecimal(1), JLong(2), JLong(3), JDecimal(4)))
		assertEquals(json""" [ 1 , ..$arg , 4 ] """, exp)
	}
	test("Splicing a range") {
		import scala.Predef.intWrapper
		val exp = JArray(List(JDecimal(1), JLong(2), JLong(3), JLong(4)))
		assertEquals(json""" [ 1, ..${2 to 4} ] """, exp)
	}
	test("Accepts nested literal arrays") {
		val arg1 = JArray(List(JDecimal(1), JDecimal(2), JDecimal(3)))
		val arg2 = JArray(List(JDecimal(4), JDecimal(5), JDecimal(6)))
		val exp = JArray(List(arg1, arg2))
		assertEquals(json""" [ [1,2,3], [4,5,6] ] """, exp)
	}

	test("Accepts a provided Map") {
		val arg = Map(("true", JBool.True), ("false", JBool.False))
		val exp = JObject(arg.toList)
		assertEquals(json"$arg", exp)
	}
	test("Accepts an empty literal Map") {
		val exp = JObject(List.empty)
		assertEquals(json"{}", exp)
	}
	test("Accepts a single-item literal Map") {
		val exp = JObject(List(("a", JDecimal(123))))
		assertEquals(json""" { "a" : 123 } """, exp)
	}
	test("Accepts a single-item key and value Map") {
		val key = "key"
		val value = "value"
		val exp = JObject(List((key, JString(value))))
		assertEquals(json""" { $key : $value } """, exp)
	}
	test("Accepts a single-item keyvalue Map") {
		val param = ("a", JLong(1))
		val exp = JObject(List(param))
		assertEquals(json""" { $param } """, exp)
	}
	test("Accepts a two-item Map with immediate values") {
		val exp = JObject(List(("a", JDecimal(123)), ("b", JNull)))
		assertEquals(json"""{"a" : 123, "b" : null}""", exp)
	}
	test("Accepts a two-item Map with variable values") {
		val k1 = "foo"
		val v1 = 123
		val k2 = "bar"
		val v2 = 456
		val kv1 = (k1, JLong(v1))
		val kv2 = (k2, JLong(v2))
		val exp = JObject(List(kv1, kv2))
		assertEquals(json"""{$k1: $v1, $k2: $v2}""", exp)
	}
	test("Accepts nested Map") {
		val arg1 = JObject(List(("a", JString("A"))))
		val arg2 = JObject(List(("b", JString("B"))))
		val exp = JObject(List(("foo", arg1), ("bar", arg2)))
		assertEquals(json"""{"foo":{"a":"A"}, "bar":{"b":"B"}}""", exp)
	}
	test("Accepts interpolation inside keys") {
		val arg = "12"
		val exp = JObject(List(("ab12cd", JNull)))
		assertEquals(json"""{"ab${arg}cd" : null}""", exp)
	}
	test("Splicing") {
		val param = Map(("a", JString("A")), ("b", JString("B")))
		val exp = JObject((param ++ Map(("c", JString("C")))).toList)
		assertEquals(json"""{ ..$param , "c" : "C" }""", exp)
	}
	test("Splicing using custom lift") {
		case class Foo(bar:Int, baz:Boolean)
		implicit val liftFoo:Lift[Foo, JObject] = Lift(x => JObject(List(("foo", JLong(x.bar)), ("baz", JBool(x.baz)))))
		val param = Foo(42, false)
		val exp = JObject(List(("_type", JString("Foo")), ("foo", JLong(42)), ("baz", JBool.False)))
		assertEquals(json"""{ "_type": "Foo", ..$param }""", exp)
	}
	test("Reject Map with non-string keys") {
		assertNotEquals(
			compileErrors(""" json"{1:2}" """),
			""
		)
	}

	test("Rejects an empty string") {
		assertEquals(
			compileErrors("""(json"")"""),
			Seq(
				"""error: Expected "[" or "\"" or "false" or "null" or "true" or "{" or Liftable Value or Number Literal""",
				"""(json"")""",
				ErrorPositionCompat.emptyString
			).mkString("\n")
		)
	}
	test("Rejects a whitespace-only string") {
		assertNotEquals(
			compileErrors(""" json"   " """),
			""
		)
	}
	test("Rejects trailing content") {
		assertNotEquals(
			compileErrors(""" json"true false" """),
			""
		)
	}
	test("Rejects string with unknown escape sequence (a)") {
		assertNotEquals(
			compileErrors(" json\"\"\" \"\\a\" \"\"\" "),
			""
		)
	}
	test("Rejects string with an invalid unicode escape sequence") {
		assertNotEquals(
			compileErrors(" json\"\"\" \"\\u12u4\" \"\"\" "),
			""
		)
	}
	test("Rejects array with object prefix/suffix") {
		assertNotEquals(
			compileErrors(""" json"{null}" """),
			""
		)
	}
	test("Rejects object with array prefix/suffix") {
		assertNotEquals(
			compileErrors(" json\"\"\"[\"\":null]\"\"\" "),
			""
		)
	}
}
