package name.rayrobdod.stringContextParserCombinator

/**
 * Describes how much a Repeat will attempt to match
 */
sealed trait RepeatStrategy

/**
 * The instances of the RepeatStrategy enum
 */
object RepeatStrategy {
	/**
	 * Match as many copies as possible, no backtracking
	 */
	object Possessive extends RepeatStrategy
	/**
	 * Match as many copies as possible
	 */
	object Greedy extends RepeatStrategy
	/**
	 * Match as few copies as possible
	 */
	object Lazy extends RepeatStrategy
}
