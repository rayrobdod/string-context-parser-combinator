package name.rayrobdod.stringContextParserCombinator

import scala.annotation.unused

object PointerSpacesCompat {
	def apply(v2:Int, @unused v3:Int) = (" " * v2) + "^"
}
