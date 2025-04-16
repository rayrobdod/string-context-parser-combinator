package name.rayrobdod.stringContextParserCombinatorExample.xml

final class TargetTypingTest extends munit.FunSuite:
	given xml: MinixmlFactory.type = MinixmlFactory

	test ("finds MinixmlFactory for target type minixml.Elem") {
		val actual: minixml.Elem = xml"<tag-name/>"
		import minixml.*
		val expected = Elem(QName("", "tag-name"), Map.empty, Seq.empty)
		assertEquals(actual, expected)
	}

	test ("finds XmlFactory.default for target type scala.xml.NodeSeq") {
		val actual: scala.xml.NodeSeq = xml"<tag-name/>"
		import scala.xml.*
		val expected = Elem(null, "tag-name", Null, TopScope, true)
		assertEquals(actual, expected)
	}
