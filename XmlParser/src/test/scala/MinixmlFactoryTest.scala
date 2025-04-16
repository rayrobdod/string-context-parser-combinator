package name.rayrobdod.stringContextParserCombinatorExample.xml

import scala.language.dynamics

package minixml:
	final case class QName(namespace: String, local:String)
	final case class Attribute(name: QName, value: String):
		def toTuple:(QName, String) = (name, value)
	trait Node
	final case class Text(content:String) extends Node
	final case class Elem(name:QName, attrs: Map[QName, String], childs: Seq[Node]) extends Node

object MinixmlFactory extends XmlFactory[minixml.Elem]:
	import minixml.*

	inline def literal(arg:Any):arg.type = arg
	inline def interpolation(arg:Any):arg.type = arg

	object elements extends Dynamic:
		def applyDynamic(name: String)(params: (Attribute | Node)*):Elem =
			val (attrs, nodes) = params.partitionMap({
				case x:Attribute => Left(x)
				case x:Node => Right(x)
			})
			Elem(QName("", name), attrs.map(_.toTuple).toMap, nodes)

	object texts extends Dynamic:
		def selectDynamic(data: String):Text =
			Text(data)

	object cdata extends Dynamic:
		def selectDynamic(data: String):Text =
			Text(data)

	val entities: XmlFactory.PredefinedEntities[Text] =
			XmlFactory.PredefinedEntities(c => Text(c.toString))

	object values extends Dynamic:
		def selectDynamic(data: String):Text =
			Text(data)

	object attributes extends Dynamic:
		def applyDynamic(name: String)(value: Text*): Attribute =
			Attribute(QName("", name), value.map(_.content).mkString)

	final class Namespaced(uri: String):
		object elements extends Dynamic:
			def applyDynamic(localName: String)(params: (Attribute | Node)*):Elem =
				val (attrs, nodes) = params.partitionMap({
					case x:Attribute => Left(x)
					case x:Node => Right(x)
				})
				Elem(QName(uri, localName), attrs.map(_.toTuple).toMap, nodes)

		object attributes extends Dynamic:
			def applyDynamic(name: String)(value: Text*): Attribute =
				Attribute(QName(uri, name), value.map(_.content).mkString)

	object uris extends Dynamic:
		def selectDynamic(uri:String):Namespaced =
			new Namespaced(uri)

		def applyDynamic(uri: String)(prefix:String):Namespaced =
			new Namespaced(uri)

final class MiniXmlFactoryTest extends munit.FunSuite {
	given xml: MinixmlFactory.type = MinixmlFactory
	import minixml.*

	test ("empty self-closing tag") {
		val expected = Elem(QName("", "tag-name"), Map.empty, Seq.empty)
		val actual = xml"<tag-name/>"
		assertEquals(actual, expected)
	}
	test ("empty tag") {
		val expected = Elem(QName("", "tag-name"), Map.empty, Seq.empty)
		val actual = xml"<tag-name></tag-name>"
		assertEquals[Any, Any](actual, expected)
	}
	test ("text innerContent") {
		val expected = Elem(QName("", "outer"), Map.empty, Seq(Text("content")))
		val actual = xml"<outer>content</outer>"
		assertEquals(actual, expected)
	}
	test ("entity innerContent") {
		val expected = Elem(QName("", "outer"), Map.empty, Seq(Text("&")))
		val actual = xml"<outer>&amp;</outer>"
		assertEquals(actual, expected)
	}
	test ("charRef innerContent") {
		val expected = Elem(QName("", "outer"), Map.empty, Seq(Text("A")))
		val actual = xml"<outer>&#65;</outer>"
		assertEquals(actual, expected)
	}
	test ("cdata innerContent") {
		val expected = Elem(QName("", "outer"), Map.empty, Seq(Text("character-data")))
		val actual = xml"<outer><![CDATA[character-data]]></outer>"
		assertEquals(actual, expected)
	}
	test ("cdata innerContent with ']]'") {
		val expected = Elem(QName("", "outer"), Map.empty, Seq(Text("character]]data")))
		val actual = xml"<outer><![CDATA[character]]data]]></outer>"
		assertEquals(actual, expected)
	}
	test ("charRef innerContent melds together with surrounding text") {
		val expected = Elem(QName("", "outer"), Map.empty, Seq(Text("  A  ")))
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
	test ("interpolated innerContent") {
		val arg = Elem(QName("", "interpolated-inner"), Map.empty, Seq())
		val expected = Elem(QName("", "outer"), Map.empty, Seq(arg))
		val actual = xml"<outer>${arg}</outer>"
		assertEquals(actual, expected)
	}
	test ("nested elements") {
		val expected =
			Elem(QName("", "outer"), Map.empty, Seq(
				Elem(QName("", "inner"), Map.empty, Seq.empty)
			))
		val actual = xml"<outer><inner /></outer>"
		assertEquals(actual, expected)
	}
	test ("attribute") {
		val expected =
			Elem(QName("", "with-attr"), Map(
				QName("", "key") -> "value",
			), Seq.empty)
		val actual = xml"<with-attr key='value' />"
		assertEquals(actual, expected)
	}
	test ("interpolated attribute") {
		val arg = Attribute(QName("tag:rayrobdod.name,2023-10:example", "attr"), "data")
		val expected =
			Elem(QName("", "with-attr"), Map(arg.toTuple), Seq.empty)
		val actual = xml"<with-attr $arg />"
		assertEquals(actual, expected)
	}
	test ("interpolated attribute value") {
		val arg = Text("interpolated value")
		val expected =
			Elem(QName("", "with-attr"), Map(QName("", "key") -> "interpolated value"), Seq.empty)
		val actual = xml"<with-attr key=$arg />"
		assertEquals(actual, expected)
	}
	test ("multiple attributes") {
		val arg = Attribute(QName("", "interpolated"), "2")
		val expected =
			Elem(QName("", "with-attrs"), Map(
				QName("", "first") -> "1",
				QName("", "interpolated") -> "2",
				QName("", "last") -> "3",
			), Seq.empty)
		val actual = xml"<with-attrs first='1' $arg last='3' />"
		assertEquals(actual, expected)
	}
	test ("prefixed attribute") {
		/* `compileErrors` being in the error message here is weird, but as long as its some munit oddity and not appearing in real uses, its whatever */
		assertNoDiff(
			compileErrors("""xml"<with-attr prefix:key='value' />""""),
				"""|error: MiniXmlFactoryTest.this.xml has no field `prefixes`
					|			compileErrors(""".stripMargin + "\"\"\"xml\"<with-attr prefix:key='value' />\"\"\"\"" + """),
					|               ^
					|""".stripMargin
		)
	}
	test ("namespaced attribute") {
		val expected =
			Elem(QName("", "with-attr"), Map(
				QName("tag:rayrobdod.name,2023-10:example", "key") -> "value",
			), Seq.empty)
		val actual = xml"<with-attr xmlns:prefix='tag:rayrobdod.name,2023-10:example' prefix:key='value' />"
		assertEquals(actual, expected)
	}
	test ("prefixed element") {
		/* `compileErrors` being in the error message here is weird, but as long as its some munit oddity and not appearing in real uses, its whatever */
		assertNoDiff(
			compileErrors("""xml"<prefix:prefixed/>""""),
				"""|error: MiniXmlFactoryTest.this.xml has no field `prefixes`
					|			compileErrors(""".stripMargin + "\"\"\"xml\"<prefix:prefixed/>\"\"\"\"" + """),
					|               ^
					|""".stripMargin
		)
	}
	test ("namespaced no-prefix element") {
		val expected =
			Elem(QName("tag:rayrobdod.name,2023-10:example", "namespaced"), Map.empty, Seq.empty)
		val actual = xml"<namespaced xmlns='tag:rayrobdod.name,2023-10:example' />"
		assertEquals(actual, expected)
	}
	test ("namespaced element") {
		val expected =
			Elem(QName("tag:rayrobdod.name,2023-10:example", "namespaced"), Map.empty, Seq.empty)
		val actual = xml"<prefix:namespaced xmlns:prefix='tag:rayrobdod.name,2023-10:example' />"
		assertEquals(actual, expected)
	}
	test ("outer namespace bindings apply to inner elements") {
		val ns = "tag:rayrobdod.name,2023-10:example"
		val expected =
			Elem(QName(ns, "outer"), Map.empty, Seq(
				Elem(QName(ns, "inner"), Map.empty, Seq.empty)
			))
		val actual =
			xml"""<outer xmlns='tag:rayrobdod.name,2023-10:example'
				><inner /></outer>"""
		assertEquals(actual, expected)
	}
	test ("outer namespace bindings apply to inner elements different namespaces") {
		val ns1 = "tag:rayrobdod.name,2023-10:example"
		val ns2 = "tag:rayrobdod.name,2023-10:example2"
		val expected =
			Elem(QName(ns1, "outer"), Map.empty, Seq(
				Elem(QName(ns2, "inner"), Map.empty, Seq.empty)
			))
		val actual =
			xml"""<ns1:outer xmlns:ns1='tag:rayrobdod.name,2023-10:example'
					xmlns:ns2='tag:rayrobdod.name,2023-10:example2'
				><ns2:inner /></ns1:outer>"""
		assertEquals(actual, expected)
	}
}
