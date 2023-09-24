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
			case ExpectingSet.NonEmpty(position, descriptions) => {
				// `sorted` to make result deterministic
				val descriptions2 = descriptions.toList.sortBy(_.toString).mkString("Expected ", " or ", "")
				q.reflect.report.errorAndAbort(descriptions2, position + 1)
			}
		}
	}
}

package stringContextParserCombinator {
	/** Support for [[Interpolator.lifted]]; represents a macro-level function that combines a CC[A] and an A. */
	trait LiftFunction[-CC[_], +Z] {def apply[A](lifter:Expr[CC[A]], elem:Expr[A])(using Type[A], Quotes):Z}
	/** An identity context - for parsing outside of a macro */
	type Id[+A] = A
	/** An identity function for lifting into the identity context */
	type IdToExpr[A] = =:=[A, A]
}
