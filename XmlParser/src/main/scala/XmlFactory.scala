package name.rayrobdod.stringContextParserCombinatorExample.xml

/**
 * A marker trait that can be implicitly picked up by XmlParser
 *
 *  - elements
 *  - attributes
 *  - values
 *  - entities
 *  - processInstructions
 *  - text
 *  - comment
 *  - cdata
 *  - prefixes
 *  - uris
 *
 * As well as methods with these names:
 *  - literal
 *  - interpolation
 */
trait XmlFactory

object XmlFactory:
	/** An `XmlFactory.entities` implementation that handles only the predefined entities */
	final class PredefinedEntities[A](fn:Char => A):
		val amp:A = fn('&')
		val lt:A = fn('<')
		val gt:A = fn('>')
		val quot:A = fn('"')
		val apos:A = fn('\'')

	given default:XmlFactory with
		import scala.language.dynamics
		enum IEitherPrefixUri:
			case PrefixOnly(prefix:String)
			case UriOnly(uri:String)
			case Both(prefix:String, uri:String)

			def prefixOrNull:String = this match
				case PrefixOnly(p) => p
				case UriOnly(_) => null
				case Both(p, _) => p

			def prependTo(rest: scala.xml.NamespaceBinding): scala.xml.NamespaceBinding =
				this match {
					case PrefixOnly(_) => rest
					case UriOnly(uri) => scala.xml.NamespaceBinding(null, uri, rest)
					case Both(prefix, uri) => scala.xml.NamespaceBinding(prefix, uri, rest)
				}

		enum Prefix:
			case SansUri(prefix:String)
			case WithUri(prefix:String, uri:String)

			def prefix:String

			def prependTo(rest: scala.xml.NamespaceBinding): scala.xml.NamespaceBinding =
				this match {
					case SansUri(_) => rest
					case WithUri(prefix, uri) => scala.xml.NamespaceBinding(prefix, uri, rest)
				}

		final case class OneAttribute(ns:Option[Prefix], localName:String, value:collection.Seq[scala.xml.Node]):
			def prependTo(rest: scala.xml.MetaData): scala.xml.Attribute =
				this match {
					case OneAttribute(None, localName, value) => scala.xml.UnprefixedAttribute(localName, value, rest)
					case OneAttribute(Some(ns), localName, value) => scala.xml.PrefixedAttribute(ns.prefix, localName, value, rest)
				}

		def interpolation(arg:Any):arg.type =
			arg

		def literal(arg:scala.xml.Node*):scala.xml.NodeSeq =
			if 1 == arg.length then
				arg(0)
			else
				scala.xml.NodeSeq.fromSeq(arg)

		object elements extends Dynamic:
			def applyDynamic(name: String)(params: (OneAttribute | scala.xml.Node)*):scala.xml.Elem =
				val (attrs, nodes) = params.partitionMap({
					case x:OneAttribute => Left(x)
					case x:scala.xml.Node => Right(x)
				})
				val attrs2 = attrs.foldRight[scala.xml.MetaData](scala.xml.Null)(_ prependTo _)
				val scope = attrs.flatMap(_.ns).foldRight[scala.xml.NamespaceBinding](scala.xml.TopScope)(_ prependTo _)
				scala.xml.Elem(null, name, attrs2, scope, true, nodes*)

		object attributes extends Dynamic:
			inline def applyDynamic(localName: String)(value: (scala.xml.Text | scala.xml.EntityRef)*):OneAttribute =
				OneAttribute(None, localName, value)

		object values extends Dynamic:
			inline def selectDynamic(data: String):scala.xml.Text =
				scala.xml.Text(data)

		object texts extends Dynamic:
			inline def selectDynamic(data: String):scala.xml.Text =
				scala.xml.Text(data)

		object cdata extends Dynamic:
			inline def selectDynamic(data: String):scala.xml.PCData =
				scala.xml.PCData(data)

		object entities extends Dynamic:
			inline def selectDynamic(name: String):scala.xml.EntityRef =
				scala.xml.EntityRef(name)

		object processInstructions extends Dynamic:
			inline def applyDynamic(target: String)(text: String):scala.xml.ProcInstr =
				scala.xml.ProcInstr(target, text)

		object comments extends Dynamic:
			inline def selectDynamic(data: String):scala.xml.Comment =
				scala.xml.Comment(data)

		trait PrefixedElements(ns: IEitherPrefixUri):
			object elements extends Dynamic:
				def applyDynamic(localName: String)(values: (OneAttribute | scala.xml.Node)*):scala.xml.Elem =
					val (attrs, nodes) = values.partitionMap({
						case x:OneAttribute => Left(x)
						case x:scala.xml.Node => Right(x)
					})
					val attrs2 = attrs.foldRight[scala.xml.MetaData](scala.xml.Null)(_ prependTo _)
					val scope = ns prependTo attrs.flatMap(_.ns).foldRight[scala.xml.NamespaceBinding](scala.xml.TopScope)(_ prependTo _)
					scala.xml.Elem(ns.prefixOrNull, localName, attrs2, scope, true, nodes*)

		trait PrefixedAttributes(ns: Prefix):
			object attributes extends Dynamic:
				def applyDynamic(localName: String)(value: (scala.xml.Text | scala.xml.EntityRef)*):OneAttribute =
					OneAttribute(Option(ns), localName, value)

		object prefixes extends Dynamic:
			def selectDynamic(prefix:String):PrefixedElements with PrefixedAttributes =
				new PrefixedElements(IEitherPrefixUri.PrefixOnly(prefix)) with PrefixedAttributes(Prefix.SansUri(prefix))

		object uris extends Dynamic:
			def selectDynamic(uri:String):PrefixedElements =
				new PrefixedElements(IEitherPrefixUri.UriOnly(uri)) {}

			def applyDynamic(uri: String)(prefix:String):PrefixedElements with PrefixedAttributes =
				new PrefixedElements(IEitherPrefixUri.Both(prefix, uri)) with PrefixedAttributes(Prefix.WithUri(prefix, uri))
