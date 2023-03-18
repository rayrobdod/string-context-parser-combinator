package com.rayrobdod.stringContextParserCombinator

/**
 * The result of a parse
 */
private[stringContextParserCombinator]
sealed trait Result[+Expr, Pos, +A] {
	/** Map the values contained in a successful result. Returns a failure as-is */
	private[stringContextParserCombinator]
	def mapValues[Z](fn:A => Z):Result[Expr, Pos, Z]
}

/**
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
	val expecting:ExpectingSet[Pos]
) {
	private[stringContextParserCombinator]
	def map[Z](fn:A => Z):Success1[Expr, Pos, Z] = Success1(fn(value), remaining, expecting)
}

/**
 * The result of a successful parse
 */
private[stringContextParserCombinator]
final case class Success[+Expr, Pos : Ordering, +A](
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
			Failure(failures.map(_.expecting).foldLeft[ExpectingSet[Pos]](ExpectingSet.empty)(_ ++ _))
		}
	}

	def +:[ExprZ >: Expr, Z >: A](newHead:Success1[ExprZ, Pos, Z]) = Success(newHead, this.choicesHead :: this.choicesTail)
	def :+[ExprZ >: Expr, Z >: A](newLast:Success1[ExprZ, Pos, Z]) = Success(this.choicesHead, this.choicesTail ::: newLast :: Nil)
	def ++[ExprZ >: Expr, Z >: A](rhs:Success[ExprZ, Pos, Z]) = Success(this.choicesHead, this.choicesTail ::: rhs.choicesHead :: rhs.choicesTail)
}

private[stringContextParserCombinator]
object Success {
	private[stringContextParserCombinator]
	def apply[Expr, Pos : Ordering, A](
		value:A,
		remaining:Input[Expr, Pos],
		expecting:ExpectingSet[Pos]
	):Success[Expr, Pos, A] = Success(Success1(value, remaining, expecting))
}

/**
 * The result of a failed parse
 *
 * @constructor
 * @param expecting a description of what the parser was expecting to find
 * @param isCut true if other OrElse branches should be ignored
 */
private[stringContextParserCombinator]
final case class Failure[Pos](
	val position:Option[Pos],
	val expecting:ExpectingSet[Pos]
) extends Result[Nothing, Pos, Nothing] {
	private[stringContextParserCombinator]
	def mapValues[Z](fn:Nothing => Z):Result[Nothing, Pos, Z] = this

	/** Returns a failure that expects either the members this expects or an element that other expects */
	private[stringContextParserCombinator]
	def or(other:Failure[Pos])(implicit ev1:Ordering[Pos]):Failure[Pos] = {
		import Ordering.Implicits.infixOrderingOps
		val newPos = this.position.zip(other.position).map(ab => ab._1 max ab._2).headOption
		new Failure(newPos, this.expecting ++ other.expecting)
	}

	/** Returns a failure that expects either the members this expects or an element from the additional set */
	def or(additional:ExpectingSet[Pos]):Failure[Pos] = new Failure(this.position, this.expecting ++ additional)

	/** Returns true if this position is greater than the provided position */
	def isPositionGt(other:Pos)(implicit ev1:Ordering[Pos]):Boolean = this.position match {
		// Treat No Position as it its position is equal to any other position
		case None => true
		case Some(position) => ev1.gt(position, other)
	}
}

private[stringContextParserCombinator]
object Failure {
	def apply[Pos](expecting:ExpectingSet[Pos]):Failure[Pos] = expecting match {
		case ExpectingSet.NonEmpty(pos, _) => new Failure(Option(pos), expecting)
		case _:ExpectingSet.Empty[Pos] => new Failure(Option.empty, expecting)
	}
}
