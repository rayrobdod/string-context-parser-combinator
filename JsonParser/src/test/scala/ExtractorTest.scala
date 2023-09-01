package name.rayrobdod.stringContextParserCombinatorExample.jsonTest

import org.json4s._
import name.rayrobdod.stringContextParserCombinatorExample.json._

final class StringContextUnapplyTest extends munit.FunSuite {
	test("can extract a whole value") {
		JNull match {
			case json"$_" => // pass
			case _ => fail("did not match")
		}
	}
	test("can match an immediate null") {
		JNull match {
			case json"null" => // pass
			case _ => fail("did not match")
		}
	}
	test("can match an immediate true") {
		JBool.True match {
			case json"true" => // pass
			case _ => fail("did not match")
		}
	}
	test("can match an immediate false") {
		JBool.False match {
			case json"false" => // pass
			case _ => fail("did not match")
		}
	}
	test("can match an immediate integer") {
		JDecimal(123) match {
			case json"123" => // pass
			case _ => fail("did not match")
		}
	}
	test("can match an immediate string") {
		JString("abc") match {
			case json""" "abc" """ => // pass
			case _ => fail("did not match")
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
			case json"[$_]" =>
				//assertEquals(_1, JDecimal(123))
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
	test("can extract the values from a one-item map") {
		JObject(List("a" -> JDecimal(123))) match {
			case json"{${(k, v)}}" =>
				assertEquals(k, "a")
				assertEquals(v, JDecimal(123))
			case _ => fail("did not match")
		}
	}
	test("can extract the values from a two-item map") {
		JObject(List("a" -> JDecimal(123), "b" -> JBool.True, "c" -> JString("abc"))) match {
			case json"{${_1}, ${_2}, ${_3}}" =>
				assertEquals(_1, "a" -> JDecimal(123))
				assertEquals(_2, "b" -> JBool.True)
				assertEquals(_3, "c" -> JString("abc"))
			case _ => fail("did not match")
		}
	}
}
