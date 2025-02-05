package name.rayrobdod.stringContextParserCombinator

private[stringContextParserCombinator]
sealed trait ExpectingSet[Pos] {
	def ++(other:ExpectingSet[Pos]):ExpectingSet[Pos]
	def mapDescriptions(fn: ExpectingDescription => ExpectingDescription):ExpectingSet[Pos]
}

private[stringContextParserCombinator]
object ExpectingSet {
	final case class NonEmpty[Pos : Ordering](
			val maxPosition:Pos,
			val descriptionsAtMaxPosition:Seq[ExpectingDescription]
	) extends ExpectingSet[Pos] {
		override def ++(other: ExpectingSet[Pos]): ExpectingSet[Pos] = {
			import Ordering.Implicits.infixOrderingOps
			other match {
				case Empty() => this
				case NonEmpty(otherMaxPosition, _) if this.maxPosition > otherMaxPosition => this
				case NonEmpty(otherMaxPosition, _) if otherMaxPosition > this.maxPosition => other
				case NonEmpty(_, otherDescriptions) => new NonEmpty[Pos](maxPosition, this.descriptionsAtMaxPosition ++ otherDescriptions)
			}
		}

		override def mapDescriptions(fn: ExpectingDescription => ExpectingDescription):ExpectingSet[Pos] = {
			new NonEmpty[Pos](maxPosition, descriptionsAtMaxPosition.map(fn))
		}

		def renderDescriptions: String =
			// `sorted` and `distinct` to make result deterministic
			descriptionsAtMaxPosition
				.view
				.map(_.render)
				.distinct
				.toList
				.sorted
				.mkString("Expected ", " or ", "")
	}

	final case class Empty[Pos]() extends ExpectingSet[Pos] {
		override def ++(other: ExpectingSet[Pos]):ExpectingSet[Pos] = other
		override def mapDescriptions(fn: ExpectingDescription => ExpectingDescription):ExpectingSet[Pos] = this
	}

	def apply[Pos : Ordering](a:Expecting[Pos]):ExpectingSet[Pos] = new NonEmpty(a.position, Seq(a.description))

	def fromSpecific[Pos : Ordering](as:Iterable[Expecting[Pos]]):ExpectingSet[Pos] = {
		if (as.nonEmpty) {
			val maxPosition = as.map(_.position).reduce({(a, b) => if (Ordering[Pos].gt(a, b)) a else b})
			new NonEmpty(maxPosition, as.collect({case ex if ex.position == maxPosition => ex.description}).toSeq)
		} else {
			new Empty
		}
	}

	def empty[Pos]:ExpectingSet[Pos] = new Empty
}
