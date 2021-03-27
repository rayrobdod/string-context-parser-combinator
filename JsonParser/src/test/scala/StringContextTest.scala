package com.rayrobdod.stringContextParserCombinatorExample.jsonTest

import scala.collection.immutable.{Map, Vector}
import org.json4s.JsonAST._
import org.scalatest.funspec.AnyFunSpec
import com.rayrobdod.stringContextParserCombinatorExample.json._

final class StringContextTest extends AnyFunSpec {
	describe("StringContext.json") {
		it ("Accepts an immediate null") {
			val exp = JNull
			assertResult(exp)(json"null")
		}
		it ("Accepts an immediate true") {
			val exp = JBool.True
			assertResult(exp)(json"true")
		}
		it ("Accepts an immediate false") {
			val exp = JBool.False
			assertResult(exp)(json"false")
		}
		it ("Accepts a scala.Boolean false") {
			val foo = false
			val exp = JBool.False
			assertResult(exp)(json"$foo")
		}
		it ("Accepts a scalajson.ast.Boolean false") {
			val exp = JBool.False
			assertResult(exp)(json"$exp")
		}

		it ("Accepts a literal 0") {
			val exp = JDecimal(0)
			assertResult(exp)(json"0")
		}
		it ("Rejects a number with a leading zero") {
			assertDoesNotCompile(""" json"012" """)
		}
		it ("Accepts a literal 1") {
			val exp = JDecimal(1)
			assertResult(exp)(json"1")
		}
		it ("Accepts a literal neg1") {
			val exp = JDecimal(-1)
			assertResult(exp)(json"-1")
		}
		it ("Accepts a literal 1.5") {
			val exp = JDecimal(1.5)
			assertResult(exp)(json"1.5")
		}
		it ("Accepts a literal 1e2") {
			val exp = JDecimal(1e2)
			assertResult(exp)(json"1e2")
		}
		it ("Accepts a literal 1e-2") {
			val exp = JDecimal(1e-2)
			assertResult(exp)(json"1e-2")
		}
		it ("Accepts a literal 1.5e+2") {
			val exp = JDecimal(15e1)
			assertResult(exp)(json"1.5e+2")
		}
		it ("Accepts a JNumber 1.5") {
			val exp:JValue with JNumber = JDecimal(1.5)
			assertResult(exp)(json"$exp")
		}
		it ("Accepts an Integer 100") {
			val param = 100
			val exp = JLong(100)
			assertResult(exp)(json"$param")
		}
		it ("Accepts an BigDecimal 4.20") {
			val param = BigDecimal("4.20")
			val exp = JDecimal(BigDecimal("4.20"))
			assertResult(exp)(json"$param")
		}
		it ("Accepts a custom lift number") {
			object Pi {}
			implicit val liftPi:Lift[Pi.type, JValue with JNumber] = Lift(_ => JDecimal(BigDecimal("3.14")))
			val exp = JDecimal(BigDecimal("3.14"))
			assertResult(exp)(json"$Pi")
		}

		it ("Accepts a String string") {
			val param = "abcd"
			val exp = JString("abcd")
			assertResult(exp)(json"$param")
		}
		it ("Accepts a JString string") {
			val exp = JString("abcd")
			assertResult(exp)(json"$exp")
		}
		it ("Accepts a custom lift string") {
			object Pi {}
			implicit val liftPi:Lift[Pi.type, JString] = Lift(_ => JString("π"))
			val exp = JString("π")
			assertResult(exp)(json"$Pi")
		}
		it ("Accepts a literal string") {
			val exp = JString("abcd")
			assertResult(exp)(json""""abcd"""")
		}
		it ("Accepts a literal empty string") {
			val exp = JString("")
			assertResult(exp)(json""" "" """)
		}
		it ("Accepts a String embedded in a string") {
			val inner = "123"
			val exp = JString("ab123cd")
			assertResult(exp)(json""""ab${inner}cd"""")
		}
		it ("Accepts a JString embedded in a string") {
			val inner = JString("123")
			val exp = JString("ab123cd")
			assertResult(exp)(json""""ab${inner}cd"""")
		}
		it ("Accepts a custom lift embedded in a string") {
			object Pi {}
			implicit val liftPi:Lift[Pi.type, JString] = Lift(_ => JString("π"))
			val exp = JString("1π2")
			assertResult(exp)(json""" "1${Pi}2" """)
		}
		it ("Accepts a literal escaped-tab string") {
			val exp = JString("\t")
			assertResult(exp)(json""" "\t" """)
		}
		it ("Accepts a literal escaped-backslash string") {
			val exp = JString("\\")
			assertResult(exp)(json""" "\\" """)
		}
		it ("Accepts a literal escaped-unicode string") {
			val exp = JString("\u1234")
			assertResult(exp)(json""" "\u1234" """)
		}

		it ("Accepts a provided Vector") {
			val arg = List(JBool.True, JBool.False)
			val exp = JArray(arg)
			assertResult(exp)(json"$arg")
		}
		it ("Accepts a provided Range") {
			import scala.Predef.intWrapper
			val exp = JArray(List(JLong(1), JLong(2), JLong(3), JLong(4)))
			assertResult(exp)(json"${1 to 4}")
		}
		it ("Accepts an literal empty array") {
			val exp = JArray(List.empty)
			assertResult(exp)(json"[]")
		}
		it ("Accepts a literal array with a single immediate value") {
			val exp = JArray(List(JDecimal(123)))
			assertResult(exp)(json" [ 123 ] ")
		}
		it ("Accepts an array with two different immediate values") {
			val exp = JArray(List(JDecimal(123), JString("abc")))
			assertResult(exp)(json""" [ 123 , "abc" ] """)
		}
		it ("Accepts an array with two variable values") {
			val arg1 = JLong(123)
			val arg2 = JString("abc")
			val exp = JArray(List(arg1, arg2))
			assertResult(exp)(json""" [ $arg1 , $arg2 ] """)
		}
		it ("Vector inside a literal vector is kept as vector") {
			val arg = List(JLong(2), JLong(3))
			val exp = JArray(List(JDecimal(1), JArray(arg), JDecimal(4)))
			assertResult(exp)(json""" [ 1 , $arg , 4 ] """)
		}
		it ("Splicing a JArray flattens the values") {
			val arg = JArray(List(JLong(2), JLong(3)))
			val exp = JArray(List(JDecimal(1), JLong(2), JLong(3), JDecimal(4)))
			assertResult(exp)(json""" [ 1 , ..$arg , 4 ] """)
		}
		it ("Splicing a vector of jvalues flattens the values") {
			val arg = Vector(JLong(2), JLong(3))
			val exp = JArray(List(JDecimal(1), JLong(2), JLong(3), JDecimal(4)))
			assertResult(exp)(json""" [ 1 , ..$arg , 4 ] """)
		}
		it ("Splicing a vector of ints flattens the values") {
			val arg = Vector(2, 3)
			val exp = JArray(List(JDecimal(1), JLong(2), JLong(3), JDecimal(4)))
			assertResult(exp)(json""" [ 1 , ..$arg , 4 ] """)
		}
		it ("Splicing a range") {
			import scala.Predef.intWrapper
			val exp = JArray(List(JDecimal(1), JLong(2), JLong(3), JLong(4)))
			assertResult(exp)(json""" [ 1, ..${2 to 4} ] """)
		}
		it ("Accepts nested literal arrays") {
			val arg1 = JArray(List(JDecimal(1), JDecimal(2), JDecimal(3)))
			val arg2 = JArray(List(JDecimal(4), JDecimal(5), JDecimal(6)))
			val exp = JArray(List(arg1, arg2))
			assertResult(exp)(json""" [ [1,2,3], [4,5,6] ] """)
		}

		it ("Accepts a provided Map") {
			val arg = Map(("true", JBool.True), ("false", JBool.False))
			val exp = JObject(arg.toList)
			assertResult(exp)(json"$arg")
		}
		it ("Accepts an empty literal Map") {
			val exp = JObject(List.empty)
			assertResult(exp)(json"{}")
		}
		it ("Accepts a single-item literal Map") {
			val exp = JObject(List(("a", JDecimal(123))))
			assertResult(exp)(json""" { "a" : 123 } """)
		}
		it ("Accepts a single-item key and value Map") {
			val key = "key"
			val value = "value"
			val exp = JObject(List((key, JString(value))))
			assertResult(exp)(json""" { $key : $value } """)
		}
		it ("Accepts a single-item keyvalue Map") {
			val param = ("a", JLong(1))
			val exp = JObject(List(param))
			assertResult(exp)(json""" { $param } """)
		}
		it ("Accepts a two-item Map with immediate values") {
			val exp = JObject(List(("a", JDecimal(123)), ("b", JNull)))
			assertResult(exp)(json"""{"a" : 123, "b" : null}""")
		}
		it ("Accepts a two-item Map with variable values") {
			val k1 = "foo"
			val v1 = 123
			val k2 = "bar"
			val v2 = 456
			val kv1 = (k1, JLong(v1))
			val kv2 = (k2, JLong(v2))
			val exp = JObject(List(kv1, kv2))
			assertResult(exp)(json"""{$k1: $v1, $k2: $v2}""")
		}
		it ("Accepts nested Map") {
			val arg1 = JObject(List(("a", JString("A"))))
			val arg2 = JObject(List(("b", JString("B"))))
			val exp = JObject(List(("foo", arg1), ("bar", arg2)))
			assertResult(exp)(json"""{"foo":{"a":"A"}, "bar":{"b":"B"}}""")
		}
		it ("Accepts interpolation inside keys") {
			val arg = "12"
			val exp = JObject(List(("ab12cd", JNull)))
			assertResult(exp)(json"""{"ab${arg}cd" : null}""")
		}
		it ("Splicing") {
			val param = Map(("a", JString("A")), ("b", JString("B")))
			val exp = JObject((param ++ Map(("c", JString("C")))).toList)
			assertResult(exp)(json"""{ ..$param , "c" : "C" }""")
		}
		it ("Splicing using custom lift") {
			case class Foo(bar:Int, baz:Boolean)
			implicit val liftFoo:Lift[Foo, JObject] = Lift(x => JObject(List(("foo", JLong(x.bar)), ("baz", JBool(x.baz)))))
			val param = Foo(42, false)
			val exp = JObject(List(("_type", JString("Foo")), ("foo", JLong(42)), ("baz", JBool.False)))
			assertResult(exp)(json"""{ "_type": "Foo", ..$param }""")
		}
		it ("Reject Map with non-string keys") {
			assertDoesNotCompile(""" json"{1:2}" """)
		}

		it ("Rejects an empty string") {
			assertDoesNotCompile(""" json"" """)
		}
		it ("Rejects a whitespace-only string") {
			assertDoesNotCompile(""" json"   " """)
		}
		it ("Rejects trailing content") {
			assertDoesNotCompile(""" json"true false" """)
		}
		it ("Rejects array with object prefix/suffix") {
			assertDoesNotCompile(""" json"{null}" """)
		}
		it ("Rejects object with array prefix/suffix") {
			assertDoesNotCompile(" json\"\"\"[\"\":null]\"\"\" ")
		}
	}
}
