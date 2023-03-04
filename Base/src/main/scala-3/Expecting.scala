package com.rayrobdod.stringContextParserCombinator

/** Represents a textual description of under what conditions a parser would return success */
private[stringContextParserCombinator]
opaque type ExpectingDescription = String

private[stringContextParserCombinator]
inline def ExpectingDescription(backing:String):ExpectingDescription = backing

extension (description:ExpectingDescription)
	private[stringContextParserCombinator]
	def where(condition:ExpectingDescription):ExpectingDescription = s"$description where $condition"

private[stringContextParserCombinator]
final case class Expecting[Pos](val description:ExpectingDescription, val position:Pos) {
	def where(condition:ExpectingDescription) = new Expecting(description.where(condition), position)
}

private[stringContextParserCombinator]
sealed trait ExpectingSet[Pos] {
	def ++(other:ExpectingSet[Pos]):ExpectingSet[Pos]
	def mapDescriptions(fn: ExpectingDescription => ExpectingDescription):ExpectingSet[Pos]
}

private[stringContextParserCombinator]
object ExpectingSet {
	final case class NonEmpty[Pos : Ordering](val maxPosition:Pos, val descriptionsAtMaxPosition:Set[ExpectingDescription]) extends ExpectingSet[Pos] {
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
	}

	final case class Empty[Pos]() extends ExpectingSet[Pos] {
		override def ++(other: ExpectingSet[Pos]):ExpectingSet[Pos] = other
		override def mapDescriptions(fn: ExpectingDescription => ExpectingDescription):ExpectingSet[Pos] = this
	}

	def apply[Pos : Ordering](a:Expecting[Pos]):ExpectingSet[Pos] = new NonEmpty(a.position, Set(a.description))

	def fromSpecific[Pos : Ordering](as:Iterable[Expecting[Pos]]):ExpectingSet[Pos] = {
		if (as.nonEmpty) {
			val maxPosition = as.map(_.position).reduce({(a, b) => if (Ordering[Pos].gt(a, b)) a else b})
			new NonEmpty(maxPosition, as.collect({case ex if ex.position == maxPosition => ex.description}).toSet)
		} else {
			new Empty
		}
	}

	def empty[Pos]:ExpectingSet[Pos] = new Empty
}
