package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Set

/**
 * The result of a parse
 * @group Input/Result
 */
private[stringContextParserCombinator]
sealed trait Result[+Expr, +A] {
}

/**
 * @group Input/Result
 *
 * @constructor
 * @param value the parsed value
 * @param remaining input that was not consumed by the parser
 * @param expecting a description of what an already-run parser could find next and still succeed
 * @param isCut true if other OrElse branches should be ignored
 */
private[stringContextParserCombinator]
final case class Success1[+Expr, +A](
	val value:A,
	val remaining:Input[Expr],
	val expecting:Set[Expecting],
	val isCut:Cut
) {
	private[stringContextParserCombinator]
	def map[Z](fn:A => Z):Success1[Expr, Z] = Success1(fn(value), remaining, expecting, isCut)
}

/**
 * The result of a successful parse
 * @group Input/Result
 */
private[stringContextParserCombinator]
final case class Success[+Expr, +A](
	choicesHead:Success1[Expr, A],
	choicesTail:List[Success1[Expr, A]] = Nil
) extends Result[Expr, A] {
	private[stringContextParserCombinator]
	def mapValues[Z](fn:A => Z):Success[Expr, Z] = this.map({(x:Success1[Expr, A]) => x.map(fn)})

	private[stringContextParserCombinator]
	def map[ExprZ >: Expr, Z](fn:Success1[Expr, A] => Success1[ExprZ, Z]):Success[ExprZ, Z] = {
		Success(fn(choicesHead), choicesTail.map(fn))
	}
	private[stringContextParserCombinator]
	def flatMap[ExprZ >: Expr, Z](fn:Success1[Expr, A] => Result[ExprZ, Z]):Result[ExprZ, Z] = {
		val (successes, failures) = (choicesHead :: choicesTail)
			.map(fn)
			.foldLeft[(List[Success1[ExprZ, Z]], List[Failure])]((Nil, Nil))({(folding, elem) => elem match {
				case Success(h, t) => ((folding._1 ::: h :: t, folding._2))
				case f:Failure => ((folding._1, f :: folding._2))
			}})
		if (successes.nonEmpty) {
			Success(successes.head, successes.tail)
		} else {
			Failure(failures.flatMap(_.expecting).toSet, failures.map(_.isCut).foldLeft[Cut](Cut.False)(_ | _))
		}
	}

	def +:[ExprZ >: Expr, Z >: A](newHead:Success1[ExprZ, Z]) = Success(newHead, this.choicesHead :: this.choicesTail)
	def :+[ExprZ >: Expr, Z >: A](newLast:Success1[ExprZ, Z]) = Success(this.choicesHead, this.choicesTail ::: newLast :: Nil)
	def ++[ExprZ >: Expr, Z >: A](rhs:Success[ExprZ, Z]) = Success(this.choicesHead, this.choicesTail ::: rhs.choicesHead :: rhs.choicesTail)
}

private[stringContextParserCombinator]
object Success {
	private[stringContextParserCombinator]
	def apply[Expr, A](
		value:A,
		remaining:Input[Expr],
		expecting:Set[Expecting],
		isCut:Cut
	):Success[Expr, A] = Success(Success1(value, remaining, expecting, isCut))
}

/**
 * The result of a failed parse
 *
 * @group Input/Result
 *
 * @constructor
 * @param expecting a description of what the parser was expecting to find
 * @param isCut true if other OrElse branches should be ignored
 */
private[stringContextParserCombinator]
final case class Failure(
	val expecting:Set[Expecting],
	val isCut:Cut
) extends Result[Nothing, Nothing] {
	private[stringContextParserCombinator]
	def or(other:Failure):Failure = new Failure(this.expecting ++ other.expecting, this.isCut | other.isCut)
}

private[stringContextParserCombinator]
object Failure {
	private[stringContextParserCombinator]
	def apply(expecting:Expecting, isCut:Cut):Failure = Failure(Set(expecting), isCut)
}
