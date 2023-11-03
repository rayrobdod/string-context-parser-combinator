package name.rayrobdod.stringContextParserCombinatorExample.xml

extension (inline sc:scala.StringContext)
	transparent inline def xml(inline args:Any*)(using factory:XmlFactory):Any =
		${XmlParser.interpolate('sc, 'args, 'factory)}
