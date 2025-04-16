package name.rayrobdod.stringContextParserCombinatorExample.xml

extension (inline sc:scala.StringContext)
	transparent inline def xml[Z](inline args:Any*)(using factory:XmlFactory[Z]):Z =
		${XmlParser.interpolate('sc, 'args, 'factory)}
