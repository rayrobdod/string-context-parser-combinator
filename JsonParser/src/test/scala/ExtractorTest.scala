package com.rayrobdod.stringContextParserCombinatorExample.jsonTest

import org.json4s._
import org.scalatest.funspec.AnyFunSpec
import com.rayrobdod.stringContextParserCombinatorExample.json._

final class StringContextUnapplyTest extends AnyFunSpec {
	describe("StringContext.json.unapply") {
		it ("can extract a whole value") {
			JNull match {
				case json"$value" => // pass
				case _ => fail()
			}
		}
		it ("can match an immediate null") {
			JNull match {
				case json"null" => // pass
				case _ => fail()
			}
		}
		it ("can match an immediate true") {
			JBool.True match {
				case json"true" => // pass
				case _ => fail()
			}
		}
		it ("can match an immediate false") {
			JBool.False match {
				case json"false" => // pass
				case _ => fail()
			}
		}
		it ("can match an immediate integer") {
			JDecimal(123) match {
				case json"123" => // pass
				case _ => fail()
			}
		}
		it ("can match an immediate string") {
			JString("abc") match {
				case json""" "abc" """ => // pass
				case _ => fail()
			}
		}
		it ("can match an immediate empty array") {
			JArray(List()) match {
				case json"[]" => // pass
				case _ => fail()
			}
		}
		it ("can extract the values from a one-item array") {
			JArray(List(JDecimal(123))) match {
				case json"[${_1}]" =>
					//assertResult(JDecimal(123))(_1)
				case _ => fail()
			}
		}
		it ("can handle a pattern with whitespace") {
			JArray(List(JDecimal(123))) match {
				case json"   [   ${_1}   ]    " =>
					assertResult(JDecimal(123))(_1)
				case _ => fail()
			}
		}
		it ("can extract the values from a three-item array") {
			JArray(List(JBool.True, JDecimal(123), JString("abc"))) match {
				case json"[${_1}, ${_2}, ${_3}]" =>
					assertResult(JBool.True)(_1)
					assertResult(JDecimal(123))(_2)
					assertResult(JString("abc"))(_3)
				case _ => fail()
			}
		}
		it ("can extract some values from a three-item array") {
			JArray(List(JBool.True, JDecimal(123), JString("abc"))) match {
				case json"[${_1}, 123, ${_3}]" =>
					assertResult(JBool.True)(_1)
					assertResult(JString("abc"))(_3)
				case _ => fail()
			}
		}
		it ("a one-elem list pattern does not match an empty list") {
			JArray(List()) match {
				case json"[${_1}]" => fail()
				case _ => // pass
			}
		}
		it ("a one-elem list pattern does not match a two-element list") {
			JArray(List(JBool.True, JBool.True)) match {
				case json"[${_1}]" => fail()
				case _ => // pass
			}
		}
		it ("can match an immediate empty object") {
			JObject(List()) match {
				case json"{}" => // pass
				case _ => fail()
			}
		}
		it ("can extract the values from a one-item map") {
			JObject(List("a" -> JDecimal(123))) match {
				case json"{${_1}}" =>
					assertResult("a" -> JDecimal(123))(_1)
				case _ => fail()
			}
		}
		it ("can extract the values from a two-item map") {
			JObject(List("a" -> JDecimal(123), "b" -> JBool.True, "c" -> JString("abc"))) match {
				case json"{${_1}, ${_2}, ${_3}}" =>
					assertResult("a" -> JDecimal(123))(_1)
					assertResult("b" -> JBool.True)(_2)
					assertResult("c" -> JString("abc"))(_3)
				case _ => fail()
			}
		}
	}
}
