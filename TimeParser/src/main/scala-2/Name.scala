package name.rayrobdod.stringContextParserCombinatorExample.datetime

private[datetime] object Name {
	def unapply(input:scala.reflect.api.Universe#Name):Option[String] =
		Option(input.decodedName.toString)
}
