package com.rayrobdod.stringContextParserCombinator

/**
 * An object that can be a pattern match pattern
 */
sealed trait Unapply[-A] {
}

/**
 * The types of pattern match objects
 */
object Unapply {
	/**
	 * An extractor that extracts zero items
	 */
	trait Zero[-A] extends Unapply[A] {
		def unapply(scrutinee:A):Boolean
	}

	/**
	 * An extractor that extracts a set number of items
	 */
	trait Fixed[-A, Z] extends Unapply[A] {
		def unapply(scrutinee:A):Option[Z]
	}

	/**
	 * An extractor that extracts a set number of items and always succeeds
	 */
	trait FixedIrrefutable[-A, Z] extends Unapply[A] {
		def unapply(scrutinee:A):Some[Z]
	}

	/**
	 * An extractor that extracts a variable number of items
	 */
	trait Seq[-A, Z] extends Unapply[A] {
		def unapplySeq(scrutinee:A):Option[scala.collection.immutable.Seq[Z]]
	}

	/**
	 * An extractor that extracts a variable number of items and always succeeds
	 */
	trait SeqIrrefutable[-A, Z] extends Unapply[A] {
		def unapplySeq(scrutinee:A):Some[scala.collection.immutable.Seq[Z]]
	}
}
