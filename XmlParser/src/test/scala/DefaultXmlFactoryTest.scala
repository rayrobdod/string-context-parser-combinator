package name.rayrobdod.stringContextParserCombinatorExample.xml

import scala.language.dynamics

final class DefaultXmlFactoryTest extends munit.FunSuite:
	val xml = XmlFactory.default

	test ("empty self-closing tag") {
		val expected = xml.literal(
			xml.elements.`tag-name`()
		)
		val actual = xml"<tag-name/>"
		assertEquals(actual, expected)
	}
	test ("empty tag") {
		val expected = xml.literal(
			xml.elements.`tag-name`()
		)
		val actual = xml"<tag-name></tag-name>"
		assertEquals(actual, expected)
	}
	test ("text innerContent") {
		val expected = xml.literal(
			xml.elements.`outer`(
				xml.texts.`content`
			)
		)
		val actual = xml"<outer>content</outer>"
		assertEquals(actual, expected)
	}
	test ("entity innerContent") {
		val expected = xml.literal(
			xml.elements.`outer`(
				xml.entities.`amp`
			)
		)
		val actual = xml"<outer>&amp;</outer>"
		assertEquals(actual, expected)
	}
	test ("entity and text mixed innerContent") {
		val expected = xml.literal(
			xml.elements.`outer`(
				xml.texts.na,
				xml.entities.`iuml`,
				xml.texts.ve,
			)
		)
		val actual = xml"<outer>na&iuml;ve</outer>"
		assertEquals(actual, expected)
	}
	test ("charRef innerContent") {
		val expected = xml.literal(
			xml.elements.`outer`(
				xml.texts.`A`
			)
		)
		val actual = xml"<outer>&#65;</outer>"
		assertEquals(actual, expected)
	}
	test ("cdata innerContent") {
		val expected = xml.literal(
			xml.elements.`outer`(
				xml.cdata.`character-data`
			)
		)
		val actual = xml"<outer><![CDATA[character-data]]></outer>"
		assertEquals(actual, expected)
	}
	test ("cdata innerContent with ']]'") {
		val expected = xml.literal(
			xml.elements.`outer`(
				xml.cdata.`character]]data`
			)
		)
		val actual = xml"<outer><![CDATA[character]]data]]></outer>"
		assertEquals(actual, expected)
	}
	test ("charRef innerContent melds together with surrounding text") {
		val expected = xml.literal(
			xml.elements.`outer`(
				xml.texts.`  A  `
			)
		)
		val actual = xml"<outer>  &#65;  </outer>"
		assertEquals(actual, expected)
	}
	test ("charRef innerContent invalid codepoint") {
		assertNoDiff(
			compileErrors("""xml"<outer>&#100000000;</outer>""""),
			"""|error: Expected valid codepoint
				|xml"<outer>&#100000000;</outer>"
				|             ^
				|""".stripMargin
		)
	}
	test ("charRef innerContent invalid codepoint 2") {
		assertNoDiff(
			compileErrors("""xml"<outer>&#1;</outer>""""),
			"""|error: Expected valid codepoint
				|xml"<outer>&#1;</outer>"
				|             ^
				|""".stripMargin
		)
	}
	test ("nested elements") {
		val expected = xml.literal(
			xml.elements.`outer`(
				xml.elements.`inner`()
			)
		)
		val actual = xml"<outer><inner /></outer>"
		assertEquals(actual, expected)
	}
	test ("attribute") {
		val expected = xml.literal(
			xml.elements.`with-attr`(
				xml.attributes.`key`(
					xml.values.`value`
				)
			)
		)
		val actual = xml"<with-attr key='value' />"
		assertEquals(actual, expected)
	}
	test ("attribute") {
		val expected = xml.literal(
			xml.elements.`with-attr`(
				xml.attributes.`key`(
					xml.values.`value`
				)
			)
		)
		val actual = xml"<with-attr key='value' />"
		assertEquals(actual, expected)
	}
	test ("prefixed attribute") {
		val expected = xml.literal(
			xml.elements.`with-attr`(
				xml.prefixes.`prefix`.attributes.`key`(
					xml.values.`value`
				)
			)
		)
		val actual = xml"<with-attr prefix:key='value' />"
		assertEquals(actual, expected)
	}
	test ("namespaced attribute") {
		val expected = xml.literal(
			xml.elements.`with-attr`(
				xml.uris.`tag:rayrobdod.name,2023-10:example`("prefix").attributes.`key`(
					xml.values.`value`
				)
			)
		)
		val actual = xml"<with-attr xmlns:prefix='tag:rayrobdod.name,2023-10:example' prefix:key='value' />"
		assertEquals(actual, expected)
	}
	test ("prefixed element") {
		val expected = xml.literal(
			xml.prefixes.`prefix`.elements.`prefixed`()
		)
		val actual = xml"<prefix:prefixed />"
		assertEquals(actual, expected)
	}
	test ("namespaced no-prefix element") {
		val expected = xml.literal(
			xml.uris.`tag:rayrobdod.name,2023-10:example`.elements.`namespaced`()
		)
		val actual = xml"<namespaced xmlns='tag:rayrobdod.name,2023-10:example' />"
		assertEquals(actual, expected)
	}
	test ("namespaced element") {
		val expected = xml.literal(
			xml.uris.`tag:rayrobdod.name,2023-10:example`("prefix").elements.`namespaced`()
		)
		val actual = xml"<prefix:namespaced xmlns:prefix='tag:rayrobdod.name,2023-10:example' />"
		assertEquals(actual, expected)
	}
	test ("comment") {
		val expected = xml.literal(
			xml.comments.` comment `
		)
		val actual = xml"<!-- comment -->"
		assertEquals(actual, expected)
	}
	test ("processing instruction") {
		val expected = xml.literal(
			xml.processInstructions.`xml-stylesheet`("""type="text/xsl" href="style.xsl"""")
		)
		val actual = xml"""<?xml-stylesheet type="text/xsl" href="style.xsl"?>"""
		assertEquals(actual, expected)
	}
	test ("literal can be a nodeseq") {
		val expected2 = scala.xml.NodeSeq.fromSeq(Seq(
			scala.xml.Elem(null, "a", scala.xml.Null, scala.xml.TopScope, true),
			scala.xml.Elem(null, "b", scala.xml.Null, scala.xml.TopScope, true),
			scala.xml.Elem(null, "c", scala.xml.Null, scala.xml.TopScope, true),
		))
		val expected = xml.literal(
			xml.elements.a(),
			xml.elements.b(),
			xml.elements.c(),
		)
		val actual = xml"""<a/><b/><c/>"""
		assertEquals(actual, expected)
		assertEquals(actual, expected2)
	}

	test ("element with namespaced attribute has namespaces") {
		val expected =
				scala.xml.NamespaceBinding("prefix", "tag:rayrobdod.name,2023-10:example", scala.xml.TopScope)
		val actual = xml"<with-attr xmlns:prefix='tag:rayrobdod.name,2023-10:example' prefix:key='value' />"
				.asInstanceOf[scala.xml.Elem]
				.scope
		assertEquals(actual, expected)
	}
	test ("namespaced element has namespaces") {
		val expected =
				scala.xml.NamespaceBinding("prefix", "tag:rayrobdod.name,2023-10:example", scala.xml.TopScope)
		val actual = xml"<prefix:namespaced xmlns:prefix='tag:rayrobdod.name,2023-10:example' />"
				.asInstanceOf[scala.xml.Elem]
				.scope
		assertEquals(actual, expected)
	}
