package name.rayrobdod.stringContextParserCombinatorExample.aggregateLiteral

extension (inline sc:scala.StringContext)
	// A parameter using `Z` is needed to force scala to decide that the `Z` is not `Nothing`
	inline def cc[Z](inline args: Any*)(using scala.reflect.ClassTag[Z]):Z =
		${MacroImpl.stringContext_cc[Z]('sc, 'args)}

case class Point(x: Int, y: Int)
case class Circle(center: Point, radius: Int, fill: String)
