package name.rayrobdod.stringContextParserCombinator

import scala.annotation.unused

object PointerSpacesCompat {
	inline def apply(@unused v2:Int, v3:Int) = (" " * v3) + "^"
}
