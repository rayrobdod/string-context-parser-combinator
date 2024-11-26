package name.rayrobdod.stringContextParserCombinator

/** Represents a textual description of under what conditions a parser would return success */
private[stringContextParserCombinator]
sealed abstract class ExpectingDescription {
	private[stringContextParserCombinator]
	def where(condition:ExpectingDescription):ExpectingDescription
	private[stringContextParserCombinator]
	def render:String
}

private[stringContextParserCombinator]
object ExpectingDescription {
	def apply(backing: String): ExpectingDescription = {
		new ExpectingDescription {
			override def where(condition:ExpectingDescription):ExpectingDescription = {
				ExpectingDescription(s"${this.render} where ${condition.render}")
			}
			override def render: String = {
				backing
			}
			override def toString: String = {
				s"ExpectingDescription.eager($backing)"
			}
		}
	}

	def delayed(backingFn: => String): ExpectingDescription = {
		new ExpectingDescription {
			private lazy val backingValue = backingFn
			override def where(condition:ExpectingDescription):ExpectingDescription = {
				ExpectingDescription.delayed(s"${this.render} where ${condition.render}")
			}
			override def render: String = {
				backingValue
			}
			override def toString: String = {
				s"ExpectingDescription.delayed(<fn>)"
			}
		}
	}
}
