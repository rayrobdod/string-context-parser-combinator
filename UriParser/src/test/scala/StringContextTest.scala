package com.rayrobdod.stringContextParserCombinatorExample.uriTest

import java.net.URI
import com.rayrobdod.stringContextParserCombinatorExample.uri._

final class StringContextTest extends munit.FunSuite {
	test("opaque") {
		val exp = new URI("foo", "bar", null)
		assertEquals(uri"foo:bar", exp)
	}
	test("opaque with space") {
		val exp = new URI("foo", " ", null)
		assertEquals(uri"foo:%20", exp)
	}
	test("opaque with space 2") {
		val exp = new URI("foo", "bar baz", null)
		assertEquals(uri"foo:bar%20baz", exp)
	}
	test("opaque with Extended Plane character") {
		val exp = new URI("foo", "\uD83D\uDE00", null)
		assertEquals(uri"foo:%F0%9F%98%80", exp)
	}
	test("opaque with fragment") {
		val exp = new URI("foo", "bar", "baz")
		assertEquals(uri"foo:bar#baz", exp)
	}
	test("opaque with opaque-part interpolation") {
		val part1 = "ab"
		val part2 = "cd"
		val exp = new URI("foo", "12abcd34", null)
		assertEquals(uri"foo:12${part1}${part2}34", exp)
	}
	test("opaque with opaque-part interpolation does not treat a \"#\" as a fragment separator") {
		val exp = new URI("foo", "12##34", "abc")
		assertEquals(uri"foo:12${"##"}34#abc", exp)
	}

	test("hierarchical full") {
		val exp = new URI("scheme", "user", "host", 80, "/path/", "query", "fragment")
		assertEquals(uri"scheme://user@host:80/path/?query#fragment", exp)
	}
	test("hierarchical without user") {
		val exp = new URI("scheme", null, "host", 80, "/path/", "query", "fragment")
		assertEquals(uri"scheme://host:80/path/?query#fragment", exp)
	}
	test("hierarchical with just scheme and host") {
		val exp = new URI("scheme", null, "host", -1, null, null, null)
		assertEquals(uri"scheme://host", exp)
	}

	test("hierarchical with Inet4Address as host") {
		val addr = java.net.InetAddress.getByAddress(Array[Byte](1,2,3,4)).asInstanceOf[java.net.Inet4Address]
		val exp = new URI("scheme", null, "1.2.3.4", -1, "/", null, null)
		assertEquals(uri"scheme://${addr}/", exp)
	}
	test("hierarchical with Inet6Address as host") {
		val addr = java.net.InetAddress.getByAddress(Array[Byte](1,2,3,4,5,6,7,8,9,20,21,22,23,24,25,26)).asInstanceOf[java.net.Inet6Address]
		val exp = new URI("scheme", null, "[102:304:506:708:914:1516:1718:191A]", -1, "/", null, null)
		assertEquals(uri"scheme://${addr}/", exp)
	}
	test("hierarchical with Literal Inet4Address as host (0.0.0.0)") {
		val exp = new URI("scheme", null, "0.0.0.0", -1, "/", null, null)
		assertEquals(uri"scheme://0.0.0.0/", exp)
	}
	test("hierarchical with Literal Inet4Address as host (1.12.127.18)") {
		val exp = new URI("scheme", null, "1.12.127.18", -1, "/", null, null)
		assertEquals(uri"scheme://1.12.127.18/", exp)
	}
	test("hierarchical with Literal Inet4Address as host (255.28.4.67)") {
		val exp = new URI("scheme", null, "255.28.4.67", -1, "/", null, null)
		assertEquals(uri"scheme://255.28.4.67/", exp)
	}
	test("hierarchical with Literal Inet6Address as host (::)") {
		val exp = new URI("scheme", null, "[::]", -1, "/", null, null)
		assertEquals(uri"scheme://[::]/", exp)
	}
	test("hierarchical with Literal Inet6Address as host (::1)") {
		val exp = new URI("scheme", null, "[::1]", -1, "/", null, null)
		assertEquals(uri"scheme://[::1]/", exp)
	}
	test("hierarchical with Literal Inet6Address as host (1:2:3:4:5:6:7:8)") {
		val exp = new URI("scheme", null, "[1:2:3:4:5:6:7:8]", -1, "/", null, null)
		assertEquals(uri"scheme://[1:2:3:4:5:6:7:8]/", exp)
	}
	test("hierarchical with Literal Inet6Address as host (1::3:4:5:6:7:8)") {
		val exp = new URI("scheme", null, "[1::3:4:5:6:7:8]", -1, "/", null, null)
		assertEquals(uri"scheme://[1::3:4:5:6:7:8]/", exp)
	}
	test("hierarchical with Literal Inet6Address as host (1:2:3:4:5:6::8)") {
		val exp = new URI("scheme", null, "[1:2:3:4:5:6::8]", -1, "/", null, null)
		assertEquals(uri"scheme://[1:2:3:4:5:6::8]/", exp)
	}
	test("Rejects hierarchical with almost Inet6Address as host (1::3:4:5:6::8)") {
		assertNoDiff(
			compileErrors("""uri"scheme://[1::3:4:5:6::8]/""""),
			"""|error: Expected "#" or "%" or "?" or CodePointIn("-_.!~*'()") or CodePointIn("/") or CodePointIn(";@&=+$,") or EOF or alphaChar or digitChar
				|uri"scheme://[1::3:4:5:6::8]/"
				|          ^
				|""".stripMargin
		)
	}

	test("hierarchical with variable port") {
		val port:Int = 58080
		val exp = new URI("scheme", null, "example.com", 58080, "/", null, null)
		assertEquals(uri"scheme://example.com:${port}/", exp)
	}
	test("hierarchical with empty port") {
		val exp = new URI("scheme", null, "example.com", -1, "/", null, null)
		assertEquals(uri"scheme://example.com:/", exp)
	}

	test("hierarchical with variable hostport") {
		val hostport = new java.net.InetSocketAddress("127.0.0.127", 80)
		val exp = new URI("scheme", null, "127.0.0.127", 80, "/", null, null)
		assertEquals(uri"scheme://${hostport}/", exp)
	}

	test("hierarchical with query") {
		val exp = new URI(null, null, null, -1, "index.html", "123", null)
		assertEquals(uri"index.html?123", exp)
	}
	test("hierarchical with query of empty") {
		val exp = new URI(null, null, null, -1, "index.html", "", null)
		assertEquals(uri"index.html?", exp)
	}
	test("hierarchical with query of single pair") {
		val pair = ("abc", "123")
		val exp = new URI(null, null, null, -1, "index.html", "abc=123", null)
		assertEquals(uri"index.html?${pair}", exp)
	}
	test("hierarchical with query of two pair") {
		val a = ("abc", "123")
		val b = ("def", "456")
		val exp = new URI(null, null, null, -1, "index.html", "abc=123&def=456", null)
		assertEquals(uri"index.html?$a&$b", exp)
	}
	test("hierarchical with query of single map") {
		val ab = scala.collection.immutable.Map(("abc", "123"), ("def", "456"))
		val exp = new URI(null, null, null, -1, "index.html", "abc=123&def=456", null)
		assertEquals(uri"index.html?$ab", exp)
	}
	test("hierarchical with query of mixed") {
		val ab = scala.collection.immutable.Map(("abc", "123"), ("def", "456"))
		val d = "789"
		val e = "jkl"
		val exp = new URI(null, null, null, -1, "index.html", "abc=123&def=456&ghi=789&jkl=101", null)
		assertEquals(uri"index.html?$ab&ghi=$d&$e=101", exp)
	}


	test("relative absolute") {
		val exp = new URI(null, null, null, -1, "/foo/bar", null, null)
		assertEquals(uri"/foo/bar", exp)
	}
	test("relative relative") {
		val exp = new URI(null, null, null, -1, "foo/bar", null, null)
		assertEquals(uri"foo/bar", exp)
	}
	test("relative network") {
		val exp = new URI(null, null, "example.com", -1, "/", null, null)
		assertEquals(uri"//example.com/", exp)
	}

	test("resolved") {
		val base = new URI("http://example.com/path/from/")
		val exp = new URI("http://example.com/path/to/")
		assertEquals(uri"${base}../to/", exp)
	}
}
