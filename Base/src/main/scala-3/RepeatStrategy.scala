package com.rayrobdod.stringContextParserCombinator

/**
 * Describes how much a Repeat will attempt to match
 */
enum RepeatStrategy:
	/**
	 * Match as many copies as possible, no backtracking
	 */
	case Possessive
	/**
	 * Match as many copies as possible
	 */
	case Greedy
	/**
	 * Match as few copies as possible
	 */
	case Lazy
