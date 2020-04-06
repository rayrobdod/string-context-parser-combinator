package com.rayrobdod.stringContextParserCombinatorExample.uriTest

import java.net.URI
import org.scalatest.funspec.AnyFunSpec
import com.rayrobdod.stringContextParserCombinatorExample.uri.UriStringContext

final class StringContextTest extends AnyFunSpec {
	describe("StringContext.uri") {
		it ("opaque") {
			val exp = new URI("foo", "bar", null)
			assertResult(exp)(uri"foo:bar")
		}
		it ("opaque with space") {
			val exp = new URI("foo", " ", null)
			assertResult(exp)(uri"foo:%20")
		}
		it ("opaque with space 2") {
			val exp = new URI("foo", "bar baz", null)
			assertResult(exp)(uri"foo:bar%20baz")
		}
		it ("opaque with Extended Plane character") {
			val exp = new URI("foo", "\uD83D\uDE00", null)
			assertResult(exp)(uri"foo:%F0%9F%98%80")
		}
		it ("opaque with fragment") {
			val exp = new URI("foo", "bar", "baz")
			assertResult(exp)(uri"foo:bar#baz")
		}
		it ("opaque with opaque-part interpolation") {
			val part1 = "ab"
			val part2 = "cd"
			val exp = new URI("foo", "12abcd34", null)
			assertResult(exp)(uri"foo:12${part1}${part2}34")
		}
		it ("opaque with opaque-part interpolation does not treat a \"#\" as a fragment separator") {
			val exp = new URI("foo", "12##34", "abc")
			assertResult(exp)(uri"foo:12${"##"}34#abc")
		}

		it ("hierarchical full") {
			val exp = new URI("scheme", "user", "host", 80, "/path/", "query", "fragment")
			assertResult(exp)(uri"scheme://user@host:80/path/?query#fragment")
		}
		it ("hierarchical without user") {
			val exp = new URI("scheme", null, "host", 80, "/path/", "query", "fragment")
			assertResult(exp)(uri"scheme://host:80/path/?query#fragment")
		}
		it ("hierarchical with just scheme and host") {
			val exp = new URI("scheme", null, "host", -1, null, null, null)
			assertResult(exp)(uri"scheme://host")
		}

		it ("hierarchical with Inet4Address as host") {
			val addr = java.net.InetAddress.getByAddress(Array[Byte](1,2,3,4)).asInstanceOf[java.net.Inet4Address]
			val exp = new URI("scheme", null, "1.2.3.4", -1, "/", null, null)
			assertResult(exp)(uri"scheme://${addr}/")
		}
		it ("hierarchical with Inet6Address as host") {
			val addr = java.net.InetAddress.getByAddress(Array[Byte](1,2,3,4,5,6,7,8,9,20,21,22,23,24,25,26)).asInstanceOf[java.net.Inet6Address]
			val exp = new URI("scheme", null, "[102:304:506:708:914:1516:1718:191A]", -1, "/", null, null)
			assertResult(exp)(uri"scheme://${addr}/")
		}
		it ("hierarchical with Literal Inet4Address as host (0.0.0.0)") {
			val exp = new URI("scheme", null, "0.0.0.0", -1, "/", null, null)
			assertResult(exp)(uri"scheme://0.0.0.0/")
		}
		it ("hierarchical with Literal Inet4Address as host (1.12.127.18)") {
			val exp = new URI("scheme", null, "1.12.127.18", -1, "/", null, null)
			assertResult(exp)(uri"scheme://1.12.127.18/")
		}
		it ("hierarchical with Literal Inet4Address as host (255.28.4.67)") {
			val exp = new URI("scheme", null, "255.28.4.67", -1, "/", null, null)
			assertResult(exp)(uri"scheme://255.28.4.67/")
		}
		it ("hierarchical with Literal Inet6Address as host (::)") {
			val exp = new URI("scheme", null, "[::]", -1, "/", null, null)
			assertResult(exp)(uri"scheme://[::]/")
		}
		it ("hierarchical with Literal Inet6Address as host (::1)") {
			val exp = new URI("scheme", null, "[::1]", -1, "/", null, null)
			assertResult(exp)(uri"scheme://[::1]/")
		}
		it ("hierarchical with Literal Inet6Address as host (1:2:3:4:5:6:7:8)") {
			val exp = new URI("scheme", null, "[1:2:3:4:5:6:7:8]", -1, "/", null, null)
			assertResult(exp)(uri"scheme://[1:2:3:4:5:6:7:8]/")
		}
		it ("hierarchical with Literal Inet6Address as host (1::3:4:5:6:7:8)") {
			val exp = new URI("scheme", null, "[1::3:4:5:6:7:8]", -1, "/", null, null)
			assertResult(exp)(uri"scheme://[1::3:4:5:6:7:8]/")
		}
		it ("hierarchical with Literal Inet6Address as host (1:2:3:4:5:6::8)") {
			val exp = new URI("scheme", null, "[1:2:3:4:5:6::8]", -1, "/", null, null)
			assertResult(exp)(uri"scheme://[1:2:3:4:5:6::8]/")
		}
		it ("Rejects hierarchical with almost Inet6Address as host (1::3:4:5:6::8)") {
			assertDoesNotCompile(""" uri"scheme://[1::3:4:5:6::8]/" """)
		}

		it ("hierarchical with variable port") {
			val port:Int = 58080
			val exp = new URI("scheme", null, "example.com", 58080, "/", null, null)
			assertResult(exp)(uri"scheme://example.com:${port}/")
		}
		it ("hierarchical with empty port") {
			val exp = new URI("scheme", null, "example.com", -1, "/", null, null)
			assertResult(exp)(uri"scheme://example.com:/")
		}

		it ("hierarchical with variable hostport") {
			val hostport = new java.net.InetSocketAddress("127.0.0.127", 80)
			val exp = new URI("scheme", null, "127.0.0.127", 80, "/", null, null)
			assertResult(exp)(uri"scheme://${hostport}/")
		}

		it ("hierarchical with query") {
			val exp = new URI(null, null, null, -1, "index.html", "123", null)
			assertResult(exp)(uri"index.html?123")
		}
		it ("hierarchical with query of empty") {
			val exp = new URI(null, null, null, -1, "index.html", "", null)
			assertResult(exp)(uri"index.html?")
		}
		it ("hierarchical with query of single pair") {
			val pair = ("abc", "123")
			val exp = new URI(null, null, null, -1, "index.html", "abc=123", null)
			assertResult(exp)(uri"index.html?${pair}")
		}
		it ("hierarchical with query of two pair") {
			val a = ("abc", "123")
			val b = ("def", "456")
			val exp = new URI(null, null, null, -1, "index.html", "abc=123&def=456", null)
			assertResult(exp)(uri"index.html?$a&$b")
		}
		it ("hierarchical with query of single map") {
			val ab = scala.collection.immutable.Map(("abc", "123"), ("def", "456"))
			val exp = new URI(null, null, null, -1, "index.html", "abc=123&def=456", null)
			assertResult(exp)(uri"index.html?$ab")
		}
		it ("hierarchical with query of mixed") {
			val ab = scala.collection.immutable.Map(("abc", "123"), ("def", "456"))
			val d = "789"
			val e = "jkl"
			val exp = new URI(null, null, null, -1, "index.html", "abc=123&def=456&ghi=789&jkl=101", null)
			assertResult(exp)(uri"index.html?$ab&ghi=$d&$e=101")
		}


		it ("relative absolute") {
			val exp = new URI(null, null, null, -1, "/foo/bar", null, null)
			assertResult(exp)(uri"/foo/bar")
		}
		it ("relative relative") {
			val exp = new URI(null, null, null, -1, "foo/bar", null, null)
			assertResult(exp)(uri"foo/bar")
		}
		it ("relative network") {
			val exp = new URI(null, null, "example.com", -1, "/", null, null)
			assertResult(exp)(uri"//example.com/")
		}

		it ("resolved") {
			val base = new URI("http://example.com/path/from/")
			val exp = new URI("http://example.com/path/to/")
			assertResult(exp)(uri"${base}../to/")
		}
	}
}
