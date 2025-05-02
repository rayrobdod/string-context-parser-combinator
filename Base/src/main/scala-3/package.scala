package name.rayrobdod

import scala.language.higherKinds
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

/**
 * A library for implementing custom string interpolation implementations using Parser Combinators
 */
package object stringContextParserCombinator {
	private[stringContextParserCombinator]
	def reportFailure(using q:Quotes)(failure:Failure[q.reflect.Position]):Nothing = {
		import PositionGivens.given

		failure.expecting match {
			case ExpectingSet.Empty() => {
				scala.quoted.quotes.reflect.report.errorAndAbort("Parsing failed")
			}
			case set @ ExpectingSet.NonEmpty(position, _) => {
				q.reflect.report.errorAndAbort(set.renderDescriptions, position + 1)
			}
		}
	}

	given IdCtx = new IdCtx()
}

package stringContextParserCombinator {
	/** Support for [[Interpolator.lifted]]; represents a macro-level function that combines a CC[A] and an A. */
	trait LiftFunction[-CC[_], +Z] {def apply[A](lifter:Expr[CC[A]], elem:Expr[A])(using Type[A], Quotes):Z}
	/** An identity context - for parsing outside of a macro */
	type Id[+A] = A
	/** An identity function for lifting into the identity context */
	type IdToExpr[A] = =:=[A, A]
	/** A context avaliable for the identity context. Essentially a `Unit`, but with a given value */
	final class IdCtx()
}
