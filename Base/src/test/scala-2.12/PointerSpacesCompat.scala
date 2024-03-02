package name.rayrobdod.stringContextParserCombinator

object PointerSpacesCompat {
	def apply(v2:Int, v3:Int) = {
		blackhole(v3)
		(" " * v2) + "^"
	}

	/** Substitute for the lack of `unused` annotation */
	private def blackhole(x:Any): Any = x
}
