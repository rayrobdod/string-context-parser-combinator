package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Set

/**
 * The result of a parse
 * @group Input/Result
 */
private[stringContextParserCombinator]
sealed trait Result[+Expr, Pos, +A] {
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
final case class Success1[+Expr, Pos, +A](
	val value:A,
	val remaining:Input[Expr, Pos],
	val expecting:Set[Expecting[Pos]],
	val isCut:Cut
) {
	private[stringContextParserCombinator]
	def map[Z](fn:A => Z):Success1[Expr, Pos, Z] = Success1(fn(value), remaining, expecting, isCut)
}

/**
 * The result of a successful parse
 * @group Input/Result
 */
private[stringContextParserCombinator]
final case class Success[+Expr, Pos, +A](
	choicesHead:Success1[Expr, Pos, A],
	choicesTail:List[Success1[Expr, Pos, A]] = Nil
) extends Result[Expr, Pos, A] {
	private[stringContextParserCombinator]
	def mapValues[Z](fn:A => Z):Success[Expr, Pos, Z] = this.map({(x:Success1[Expr, Pos, A]) => x.map(fn)})

	private[stringContextParserCombinator]
	def map[ExprZ >: Expr, Z](fn:Success1[Expr, Pos, A] => Success1[ExprZ, Pos, Z]):Success[ExprZ, Pos, Z] = {
		Success(fn(choicesHead), choicesTail.map(fn))
	}
	private[stringContextParserCombinator]
	def flatMap[ExprZ >: Expr, Z](fn:Success1[Expr, Pos, A] => Result[ExprZ, Pos, Z]):Result[ExprZ, Pos, Z] = {
		val (successes, failures) = (choicesHead :: choicesTail)
			.map(fn)
			.foldLeft[(List[Success1[ExprZ, Pos, Z]], List[Failure[Pos]])]((Nil, Nil))({(folding, elem) => elem match {
				case Success(h, t) => ((folding._1 ::: h :: t, folding._2))
				case f:Failure[Pos] => ((folding._1, f :: folding._2))
			}})
		if (successes.nonEmpty) {
			Success(successes.head, successes.tail)
		} else {
			Failure(failures.flatMap(_.expecting).toSet, failures.map(_.isCut).foldLeft[Cut](Cut.False)(_ | _))
		}
	}

	def +:[ExprZ >: Expr, Z >: A](newHead:Success1[ExprZ, Pos, Z]) = Success(newHead, this.choicesHead :: this.choicesTail)
	def :+[ExprZ >: Expr, Z >: A](newLast:Success1[ExprZ, Pos, Z]) = Success(this.choicesHead, this.choicesTail ::: newLast :: Nil)
	def ++[ExprZ >: Expr, Z >: A](rhs:Success[ExprZ, Pos, Z]) = Success(this.choicesHead, this.choicesTail ::: rhs.choicesHead :: rhs.choicesTail)
}

private[stringContextParserCombinator]
object Success {
	private[stringContextParserCombinator]
	def apply[Expr, Pos, A](
		value:A,
		remaining:Input[Expr, Pos],
		expecting:Set[Expecting[Pos]],
		isCut:Cut
	):Success[Expr, Pos, A] = Success(Success1(value, remaining, expecting, isCut))
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
final case class Failure[Pos](
	val expecting:Set[Expecting[Pos]],
	val isCut:Cut
) extends Result[Nothing, Pos, Nothing] {
	private[stringContextParserCombinator]
	def or(other:Failure[Pos]):Failure[Pos] = new Failure(this.expecting ++ other.expecting, this.isCut | other.isCut)
}
