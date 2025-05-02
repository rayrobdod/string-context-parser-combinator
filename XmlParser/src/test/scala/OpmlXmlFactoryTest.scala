package name.rayrobdod.stringContextParserCombinatorExample.xml

package Opml:
	trait Value:
		def stringContent: String

	enum VersionValue extends Value:
		case `1.0`
		case `2.0`
		def stringContent = this match {
			case `1.0` => "1.0"
			case `2.0` => "2.0"
		}

	enum BooleanValue extends Value:
		case `true`
		case `false`
		def stringContent = this match {
			case `true` => "true"
			case `false` => "false"
		}

	case class OpmlTag(
		version:VersionValue,
		head: HeadTag,
		body:BodyTag,
	)

	case class GenericValue(stringContent: String) extends Value

	case class HeadTag(metadata: Seq[MetadataTag])

	sealed trait MetadataTag
	case class TitleTag(content: String) extends MetadataTag
	case class DateCreatedTag(content: String) extends MetadataTag
	case class DateModifiedTag(content: String) extends MetadataTag

	case class BodyTag(children: Seq[OutlineTag])
	case class OutlineTag(text:String, typ:Option[String], children: Seq[OutlineTag])


object OpmlXmlFactory extends XmlFactory[Any]:
	import Opml.*
	import scala.language.dynamics

	sealed trait OutlineAttribute
	case class TextAttribute(value:String) extends OutlineAttribute
	case class TypeAttribute(value:String) extends OutlineAttribute

	def literal(arg:Any):arg.type = arg
	def interpolation(arg:Any):arg.type = arg

	object elements:
		def opml(version:VersionValue, head:HeadTag, body:BodyTag): OpmlTag = OpmlTag(version, head, body)

		def head(metadata:MetadataTag*): HeadTag = HeadTag(metadata)
		def title(content: String*): TitleTag = TitleTag(content.mkString)
		def dateCreated(content: String*): DateCreatedTag = DateCreatedTag(content.mkString)
		def dateModified(content: String*): DateModifiedTag = DateModifiedTag(content.mkString)

		def body(children:OutlineTag*): BodyTag = BodyTag(children)
		def outline(text: TextAttribute, datas:(OutlineAttribute | OutlineTag)*): OutlineTag = {
			val (attrs, children) = datas.partitionMap({
				case x:OutlineAttribute => Left(x)
				case x:OutlineTag => Right(x)
			})
			val typ = attrs.collectFirst({case TypeAttribute(x) => x})
			OutlineTag(text.value, typ, children)
		}

	object texts extends Dynamic:
		def selectDynamic(data: String):String =
			data

	object attributes:
		def version(data: VersionValue): VersionValue = data
		def text(data: Value): TextAttribute = TextAttribute(data.stringContent)
		def `type`(data: Value): TypeAttribute = TypeAttribute(data.stringContent)

	object values extends Dynamic:
		val `1.0`: VersionValue = VersionValue.`1.0`
		val `2.0`: VersionValue = VersionValue.`2.0`
		val `true`: BooleanValue = BooleanValue.`true`
		val `false`: BooleanValue = BooleanValue.`false`
		def selectDynamic(content: String):Value = GenericValue(content)

	val entities = XmlFactory.PredefinedEntities[String](_.toString)


final class OpmlXmlFactoryTest extends munit.FunSuite {
	given OpmlXmlFactory.type = OpmlXmlFactory
	import Opml.*

	test ("valid title tag") {
		val expected = TitleTag("my title")
		val actual = xml"<title>my title</title>"
		assertEquals(actual, expected)
	}
	test ("empty body tag") {
		val expected = BodyTag(Nil)
		val actual = xml"<body></body>"
		assertEquals(actual, expected)
	}
	test ("empty head tag") {
		val expected = HeadTag(Nil)
		val actual = xml"<head></head>"
		assertEquals(actual, expected)
	}
	test ("head tag with title tag") {
		val expected = HeadTag(Seq(TitleTag("my title")))
		val actual = xml"<head><title>my title</title></head>"
		assertEquals(actual, expected)
	}
	test ("empty opml tag fails") {
		/* `compileErrors` being in the error message here is weird, but as long as its some munit oddity and not appearing in real uses, its whatever */
		assertNoDiff(
			compileErrors("""xml"<opml></opml>""""),
			"""|error: OpmlXmlFactoryTest.this.given_OpmlXmlFactory_type.elements has no method `opml` accepting ()
				|			compileErrors(""".stripMargin + "\"\"\"xml\"<opml></opml>\"\"\"\"" + """),
				|               ^
				|""".stripMargin
		)
	}
	test ("minimal opml tag") {
		val expected = OpmlTag(
			VersionValue.`1.0`,
			HeadTag(Seq.empty),
			BodyTag(Seq.empty),
		)
		val actual = xml"<opml version='1.0'><head /><body /></opml>"
		assertEquals(actual, expected)
	}
	test ("interpolated opml tag") {
		val expected = OpmlTag(
			VersionValue.`1.0`,
			HeadTag(Seq.empty),
			BodyTag(Seq.empty),
		)
		val actual = xml"<opml ${VersionValue.`1.0`}>${HeadTag(Seq.empty)}${BodyTag(Seq.empty)}</opml>"
		assertEquals(actual, expected)
	}
	test ("interpolated opml tag 2") {
		/*
		 * Interpolating a tag in the attribute position maybe shouldn't work,
		 * but I don't see a way to make it not work
		 * without making the `elements` methods have two parameter lists
		 */
		val expected = OpmlTag(
			VersionValue.`1.0`,
			HeadTag(Seq.empty),
			BodyTag(Seq.empty),
		)
		val actual = xml"<opml ${VersionValue.`1.0`} ${HeadTag(Seq.empty)} ${BodyTag(Seq.empty)} />"
		assertEquals(actual, expected)
	}
	test ("empty outline tag fails") {
		/* `compileErrors` being in the error message here is weird, but as long as its some munit oddity and not appearing in real uses, its whatever */
		assertNoDiff(
			compileErrors("""xml"<outline />""""),
			"""|error: OpmlXmlFactoryTest.this.given_OpmlXmlFactory_type.elements has no method `outline` accepting ()
				|			compileErrors(""".stripMargin + "\"\"\"xml\"<outline />\"\"\"\"" + """),
				|               ^
				|""".stripMargin
		)
	}
	test ("head tag inside outline tag fails") {
		/* `compileErrors` being in the error message here is weird, but as long as its some munit oddity and not appearing in real uses, its whatever */
		assertNoDiff(
			compileErrors("""xml"<outline text='section'><head /></outline>"""")
					.linesWithSeparators.take(16).mkString,
			"""|error:
				|Found:    name.rayrobdod.stringContextParserCombinatorExample.xml.Opml.HeadTag*
				|Required: (
				|  name.rayrobdod.stringContextParserCombinatorExample.xml.OpmlXmlFactory.
				|    OutlineAttribute
				| | name.rayrobdod.stringContextParserCombinatorExample.xml.Opml.OutlineTag)*
				|			compileErrors(""".stripMargin + "\"\"\"" + """xml"<outline text='section'><head /></outline>""".stripMargin + "\"\"\"\"" + """)
				|               ^
				|error:
				|Found:    name.rayrobdod.stringContextParserCombinatorExample.xml.Opml.HeadTag*
				|Required: (
				|  name.rayrobdod.stringContextParserCombinatorExample.xml.OpmlXmlFactory.
				|    OutlineAttribute
				| | name.rayrobdod.stringContextParserCombinatorExample.xml.Opml.OutlineTag)*
				|			compileErrors(""".stripMargin + "\"\"\"" + """xml"<outline text='section'><head /></outline>""".stripMargin + "\"\"\"\"" + """)
				|               ^
				|""".stripMargin
		)
	}
	test ("minimal outline tag") {
		val expected = OutlineTag("section", None, Nil)
		val actual = xml"<outline text='section' />"
		assertEquals(actual, expected)
	}
	test ("fuller opml tag") {
		val expected = OpmlTag(
			VersionValue.`1.0`,
			HeadTag(Seq(
				TitleTag("My Feed"),
				DateCreatedTag("2001-02-03"),
				DateModifiedTag("2001-02-03"),
			)),
			BodyTag(Seq(
				OutlineTag("first thing", None, Nil),
				OutlineTag("second thing", None, Seq(
					OutlineTag("subheading of the second thing", None, Nil),
					OutlineTag("another of the second thing", None, Nil),
				)),
				OutlineTag("third thing", None, Nil),
				OutlineTag("one final thing", None, Nil),
			)),
		)
		val actual = xml"""<opml version='1.0'
				><head
					><title>My Feed</title
					><dateCreated>2001-02-03</dateCreated
					><dateModified>2001-02-03</dateModified
				></head
				><body
					><outline text="first thing"></outline
					><outline text="second thing"
						><outline text="subheading of the second thing"></outline
						><outline text="another of the second thing"></outline
					></outline
					><outline text="third thing"></outline
					><outline text="one final thing"></outline
				></body
			></opml>"""
		assertEquals(actual, expected)
	}
}
