package com.rayrobdod.stringContextParserCombinatorExample.jsonTest

import scala.collection.immutable.{Map, Vector}
import scalajson.ast._
import org.scalatest.funspec.AnyFunSpec
import com.rayrobdod.stringContextParserCombinatorExample.json.JsonStringContext
import com.rayrobdod.stringContextParserCombinatorExample.json.Lift

final class StringContextTest extends AnyFunSpec {
	describe("StringContext.json") {
		it ("Accepts an immediate null") {
			val exp = JNull
			assertResult(exp)(json"null")
		}
		it ("Accepts an immediate true") {
			val exp = JTrue
			assertResult(exp)(json"true")
		}
		it ("Accepts an immediate false") {
			val exp = JFalse
			assertResult(exp)(json"false")
		}
		it ("Accepts a scala.Boolean false") {
			val foo = false
			val exp = JFalse
			assertResult(exp)(json"$foo")
		}
		it ("Accepts a scalajson.ast.Boolean false") {
			val exp = JFalse
			assertResult(exp)(json"$exp")
		}
		it ("Accepts a literal 0") {
			val exp = JNumber(0)
			assertResult(exp)(json"0")
		}
		it ("Rejects a number with a leading zero") {
			assertDoesNotCompile(""" json"012" """)
		}
		it ("Accepts a literal 1") {
			val exp = JNumber(1)
			assertResult(exp)(json"1")
		}
		it ("Accepts a literal neg1") {
			val exp = JNumber(-1)
			assertResult(exp)(json"-1")
		}
		it ("Accepts a literal 1.5") {
			val exp = JNumber(1.5)
			assertResult(exp)(json"1.5")
		}
		it ("Accepts a literal 1e2") {
			val exp = JNumber(1e2)
			assertResult(exp)(json"1e2")
		}
		it ("Accepts a literal 1e-2") {
			val exp = JNumber(1e-2)
			assertResult(exp)(json"1e-2")
		}
		it ("Accepts a literal 1.5e+2") {
			val exp = JNumber(15e1)
			assertResult(exp)(json"1.5e+2")
		}
		it ("Accepts a JNumber 1.5") {
			val exp:JNumber = JNumber(1.5)
			assertResult(exp)(json"$exp")
		}
		it ("Accepts an Integer 100") {
			val param = 100
			val exp = JNumber(100)
			assertResult(exp)(json"$param")
		}
		it ("Accepts an BigDecimal 4.20") {
			val param = BigDecimal("4.20")
			val exp = JNumber.fromString("4.20").get
			assertResult(exp)(json"$param")
		}
		it ("Accepts a custom lift") {
			object Pi {}
			implicit val liftPi:Lift[Pi.type, JNumber] = Lift(_ => JNumber.fromString("3.14").get)
			val exp = JNumber.fromString("3.14").get
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
		it ("Accepts a literal escaped-tab string") {
			val exp = JString("\t")
			assertResult(exp)(json""" "\t" """)
		}

		it ("Accepts a provided Vector") {
			val arg = Vector(JTrue, JFalse)
			val exp = JArray(arg)
			assertResult(exp)(json"$arg")
		}
		it ("Accepts a provided Range") {
			import scala.Predef.intWrapper
			val exp = JArray(Vector(JNumber(1), JNumber(2), JNumber(3), JNumber(4)))
			assertResult(exp)(json"${1 to 4}")
		}
		it ("Accepts an literal empty array") {
			val exp = JArray(Vector.empty)
			assertResult(exp)(json"[]")
		}
		it ("Accepts a literal array with a single immediate value") {
			val exp = JArray(Vector(JNumber(123)))
			assertResult(exp)(json" [ 123 ] ")
		}
		it ("Accepts an array with two different immediate values") {
			val exp = JArray(Vector(JNumber(123), JString("abc")))
			assertResult(exp)(json""" [ 123 , "abc" ] """)
		}
		it ("Accepts an array with two variable values") {
			val arg1 = JNumber(123)
			val arg2 = JString("abc")
			val exp = JArray(Vector(arg1, arg2))
			assertResult(exp)(json""" [ $arg1 , $arg2 ] """)
		}
		it ("Vector inside a literal vector is kept as vector") {
			val arg = Vector(JNumber(2), JNumber(3))
			val exp = JArray(Vector(JNumber(1), JArray(arg), JNumber(4)))
			assertResult(exp)(json""" [ 1 , $arg , 4 ] """)
		}
		it ("Splicing a JArray flattens the values") {
			val arg = JArray(Vector(JNumber(2), JNumber(3)))
			val exp = JArray(Vector(JNumber(1), JNumber(2), JNumber(3), JNumber(4)))
			assertResult(exp)(json""" [ 1 , ..$arg , 4 ] """)
		}
		it ("Splicing a vector of jvalues flattens the values") {
			val arg = Vector(JNumber(2), JNumber(3))
			val exp = JArray(Vector(JNumber(1), JNumber(2), JNumber(3), JNumber(4)))
			assertResult(exp)(json""" [ 1 , ..$arg , 4 ] """)
		}
		it ("Splicing a vector of ints flattens the values") {
			val arg = Vector(2, 3)
			val exp = JArray(Vector(JNumber(1), JNumber(2), JNumber(3), JNumber(4)))
			assertResult(exp)(json""" [ 1 , ..$arg , 4 ] """)
		}
		it ("Splicing a range") {
			import scala.Predef.intWrapper
			val exp = JArray(Vector(JNumber(1), JNumber(2), JNumber(3), JNumber(4)))
			assertResult(exp)(json""" [ 1, ..${2 to 4} ] """)
		}
		it ("Accepts nested literal arrays") {
			val arg1 = JArray(Vector(JNumber(1), JNumber(2), JNumber(3)))
			val arg2 = JArray(Vector(JNumber(4), JNumber(5), JNumber(6)))
			val exp = JArray(Vector(arg1, arg2))
			assertResult(exp)(json""" [ [1,2,3], [4,5,6] ] """)
		}

		it ("Accepts a provided Map") {
			val arg = Map(("true", JTrue), ("false", JFalse))
			val exp = JObject(arg)
			assertResult(exp)(json"$arg")
		}
		it ("Accepts an empty literal Map") {
			val exp = JObject(Map.empty)
			assertResult(exp)(json"{}")
		}
		it ("Accepts a single-item literal Map") {
			val exp = JObject(Map(("a", JNumber(123))))
			assertResult(exp)(json""" { "a" : 123 } """)
		}
		it ("Accepts a single-item key and value Map") {
			val key = "key"
			val value = "value"
			val exp = JObject(Map((key, JString(value))))
			assertResult(exp)(json""" { $key : $value } """)
		}
		it ("Accepts a single-item keyvalue Map") {
			val param = ("a", JNumber(1))
			val exp = JObject(Map(param))
			assertResult(exp)(json""" { $param } """)
		}
		it ("Accepts a two-item Map with immediate values") {
			val exp = JObject(Map(("a", JNumber(123)), ("b", JNull)))
			assertResult(exp)(json"""{"a" : 123, "b" : null}""")
		}
		it ("Accepts a two-item Map with variable values") {
			val k1 = "foo"
			val v1 = 123
			val k2 = "bar"
			val v2 = 456
			val kv1 = (k1, JNumber(v1))
			val kv2 = (k2, JNumber(v2))
			val exp = JObject(Map(kv1, kv2))
			assertResult(exp)(json"""{$k1: $v1, $k2: $v2}""")
		}
		it ("Accepts nested Map") {
			val arg1 = JObject(Map(("a", JString("A"))))
			val arg2 = JObject(Map(("b", JString("B"))))
			val exp = JObject(Map(("foo", arg1), ("bar", arg2)))
			assertResult(exp)(json"""{"foo":{"a":"A"}, "bar":{"b":"B"}}""")
		}
		it ("Accepts interpolation inside keys") {
			val arg = "12"
			val exp = JObject(Map(("ab12cd", JNull)))
			assertResult(exp)(json"""{"ab${arg}cd" : null}""")
		}
		it ("Splicing") {
			val param = Map(("a", JString("A")), ("b", JString("B")))
			val exp = JObject(param ++ Map(("c", JString("C"))))
			assertResult(exp)(json"""{ ..$param , "c" : "C" }""")
		}
		it ("Splicing using custom lift") {
			case class Foo(bar:Int, baz:Boolean)
			implicit val liftFoo:Lift[Foo, JObject] = Lift(x => JObject(Map(("foo", JNumber(x.bar)), ("baz", JBoolean(x.baz)))))
			val param = Foo(42, false)
			val exp = JObject(Map(("_type", JString("Foo")), ("foo", JNumber(42)), ("baz", JFalse)))
			assertResult(exp)(json"""{ "_type": "Foo", ..$param }""")
		}
		it ("Reject Map with non-string keys") {
			assertDoesNotCompile(""" json"{1:2}" """)
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
