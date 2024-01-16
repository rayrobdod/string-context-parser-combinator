package name.rayrobdod.stringContextParserCombinatorExample.jsonTest

import org.json4s._
import name.rayrobdod.stringContextParserCombinatorExample.json._

final class StringContextUnapplyTest extends munit.FunSuite {
	val sampleJValues = Seq(
		JNull,
		JBool.True,
		JBool.False,
		JDecimal(123),
		JString("abc"),
		JArray(Nil),
		JObject(Nil),
	)

	for (value <- sampleJValues) {
		test(s"irrefutable whole-value pattern matches and extracts ${value}") {
			value match {
				case json"${_1}" =>
					assertEquals(_1, value)
				case _ => fail("did not match")
			}
		}
	}
	for (value <- sampleJValues) {
		test(s"null-literal pattern ${if (value == JNull) {"matches"} else {"does not match"}} ${value}") {
			value match {
				case json"null" => if (value != JNull) {fail("did match")}
				case _ => if (value == JNull) {fail("did not match")}
			}
		}
	}
	for (value <- sampleJValues) {
		test(s"true-literal pattern ${if (value == JBool.True) {"matches"} else {"does not match"}} ${value}") {
			value match {
				case json"true" => if (value != JBool.True) {fail("did match")}
				case _ => if (value == JBool.True) {fail("did not match")}
			}
		}
	}
	for (value <- sampleJValues) {
		test(s"false-literal pattern ${if (value == JBool.False) {"matches"} else {"does not match"}} ${value}") {
			value match {
				case json"false" => if (value != JBool.False) {fail("did match")}
				case _ => if (value == JBool.False) {fail("did not match")}
			}
		}
	}
	for (value <- sampleJValues) {
		test(s"number 123 pattern ${if (value == JDecimal(123)) {"matches"} else {"does not match"}} ${value}") {
			value match {
				case json"123" => if (value != JDecimal(123)) {fail("did match")}
				case _ => if (value == JDecimal(123)) {fail("did not match")}
			}
		}
	}
	test("number 123 pattern does not match other number values") {
		JDecimal(456) match {
			case json"""123""" => fail("did match")
			case _ => // pass
		}
	}
	for (value <- Seq(JDouble(123d), JInt(scala.math.BigInt("123")), JLong(123L))) {
		test(s"number 123 pattern matches other types of numbers (${value})") {
			value match {
				case json"""123""" => // pass
				case _ => fail("did not match")
			}
		}
	}
	for (value <- sampleJValues) {
		test(s"""string "abc" pattern ${if (value == JString("abc")) {"matches"} else {"does not match"}} ${value}""") {
			value match {
				case json""" "abc" """ => if (value != JString("abc")) {fail("did match")}
				case _ => if (value == JString("abc")) {fail("did not match")}
			}
		}
	}
	test("string \"abc\" pattern does not match other strings") {
		JString("def") match {
			case json""" "abc" """ => fail("did match")
			case _ => // pass
		}
	}
	test("can match an immediate empty array") {
		JArray(List()) match {
			case json"[]" => // pass
			case _ => fail("did not match")
		}
	}
	test("can extract the values from a one-item array") {
		JArray(List(JDecimal(123))) match {
			case json"[${_1}]" =>
				assertEquals(_1, JDecimal(123))
			case _ => fail("did not match")
		}
	}
	test("can handle a pattern with whitespace") {
		JArray(List(JDecimal(123))) match {
			case json"   [   ${_1}   ]    " =>
				assertEquals(_1, JDecimal(123))
			case _ => fail("did not match")
		}
	}
	test("can extract the values from a three-item array") {
		JArray(List(JBool.True, JDecimal(123), JString("abc"))) match {
			case json"[${_1}, ${_2}, ${_3}]" =>
				assertEquals(_1, JBool.True)
				assertEquals(_2, JDecimal(123))
				assertEquals(_3, JString("abc"))
			case _ => fail("did not match")
		}
	}
	test("can extract some values from a three-item array") {
		JArray(List(JBool.True, JDecimal(123), JString("abc"))) match {
			case json"[${_1}, 123, ${_3}]" =>
				assertEquals(_1, JBool.True)
				assertEquals(_3, JString("abc"))
			case _ => fail("did not match")
		}
	}
	test("a one-elem list pattern does not match an empty list") {
		JArray(List()) match {
			case json"[$_]" => fail("did match")
			case _ => // pass
		}
	}
	test("a one-elem list pattern does not match a two-element list") {
		JArray(List(JBool.True, JBool.True)) match {
			case json"[$_]" => fail("did match")
			case _ => // pass
		}
	}
	test("can match an immediate empty object") {
		JObject(List()) match {
			case json"{}" => // pass
			case _ => fail("did not match")
		}
	}
	test("can extract the key-value pairs from a one-item map") {
		JObject(List("a" -> JDecimal(123))) match {
			case json"{${(k, v)}}" =>
				assertEquals(k, "a")
				assertEquals(v, JDecimal(123))
			case _ => fail("did not match")
		}
	}
	test("can extract the key-value pairs from a three-item map") {
		JObject(List("a" -> JDecimal(123), "b" -> JBool.True, "c" -> JString("abc"))) match {
			case json"{${_1}, ${_2}, ${_3}}" =>
				assertEquals(_1, "a" -> JDecimal(123))
				assertEquals(_2, "b" -> JBool.True)
				assertEquals(_3, "c" -> JString("abc"))
			case _ => fail("did not match")
		}
	}
}
