package name.rayrobdod.stringContextParserCombinator

private[stringContextParserCombinator]
final case class Expecting[Pos](
	val description:ExpectingDescription,
	val position:Pos,
) {
	def where(condition:ExpectingDescription) =
		new Expecting(description.where(condition), position)
}
