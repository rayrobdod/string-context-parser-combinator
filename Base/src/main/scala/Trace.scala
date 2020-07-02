package com.rayrobdod.stringContextParserCombinator

/**
 * A representation of how a result came to be
 * @group Input/Result
 */
sealed trait Trace[+Expr] {
	private[stringContextParserCombinator] def removeRequiredThens:Trace[Expr]
	private[stringContextParserCombinator] def expectingDescription:String
	private[stringContextParserCombinator] def leftMostRemaining:Input[Expr]
}

/**
 * @group Input/Result
 */
private[stringContextParserCombinator]
final case class EmptyTrace[+Expr](remaining:Input[Expr]) extends Trace[Expr] {
	def removeRequiredThens:Trace[Expr] = this
	def expectingDescription:String = "∅"
	def leftMostRemaining:Input[Expr] = remaining
}

/**
 * @group Input/Result
 */
private[stringContextParserCombinator]
final case class LeafTrace[+Expr](parser:Expecting, remaining:Input[Expr]) extends Trace[Expr] {
	def removeRequiredThens:Trace[Expr] = this
	def expectingDescription:String = parser.description
	def leftMostRemaining:Input[Expr] = remaining
}

/**
 * @group Input/Result
 */
private[stringContextParserCombinator]
final case class OrTrace[+Expr](left:Trace[Expr], right:Trace[Expr]) extends Trace[Expr] {
	def removeRequiredThens:Trace[Expr] = this
	def expectingDescription:String = {
		def impl(x:Trace[Expr]):String = x match {
			case _:ThenTrace[_] | _:FilterTrace[_] => s"(${x.expectingDescription})"
			case _ => x.expectingDescription
		}
		s"${impl(left)} | ${impl(right)}"
	}
	def leftMostRemaining:Input[Expr] = {
		// in theory, `left.leftMostRemaining == right.leftMostRemaining`
		left.leftMostRemaining
	}
}

/**
 * @group Input/Result
 */
private[stringContextParserCombinator]
final case class ThenTrace[+Expr](left:Trace[Expr], right:Trace[Expr]) extends Trace[Expr] {
	def removeRequiredThens:Trace[Expr] = right
	def expectingDescription:String = {
		def impl(x:Trace[Expr]):String = x match {
			case _:OrTrace[_] | _:FilterTrace[_] => s"(${x.expectingDescription})"
			case _ => x.expectingDescription
		}
		s"${impl(left)} ~ ${impl(right)}"
	}
	def leftMostRemaining:Input[Expr] = left.leftMostRemaining
}

/**
 * @group Input/Result
 */
private[stringContextParserCombinator]
final case class FilterTrace[+Expr](filter:Expecting, backing:Trace[Expr]) extends Trace[Expr] {
	def removeRequiredThens:Trace[Expr] = this
	def expectingDescription:String = {
		def impl(x:Trace[Expr]):String = x match {
			case _:OrTrace[_] | _:ThenTrace[_] => s"(${x.expectingDescription})"
			case _ => x.expectingDescription
		}
		s"${impl(backing)} where ${filter.description}"
	}
	def leftMostRemaining:Input[Expr] = backing.leftMostRemaining
}
