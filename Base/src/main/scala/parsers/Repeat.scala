package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class Repeat[Expr, A, Z](
	inner:Parser[Expr, A],
	min:Int,
	max:Int,
	delimiter:Parser[Expr, Unit],
	strategy:RepeatStrategy,
	ev:typelevel.Repeated[A, Z]
) extends AbstractParser[Expr, Z] {
	require(min >= 0)
	require(max >= 1)
	require(max >= min)

	def parse[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos]):Result[ExprZ, Pos, Z] = {
		Repeat.parse0(input, inner, min, max, delimiter, strategy, true) match {
			case f:Failure[Pos] => f
			case s:Success[ExprZ, Pos, List[A]] => s.mapValues({parts =>
				val acc = ev.init()
				parts.foreach(part => ev.append(acc, part))
				ev.result(acc)
			})
		}
	}
}

private[stringContextParserCombinator]
object Repeat {
	private def parse0[Expr, Pos, A](
		input:Input[Expr, Pos],
		inner:Parser[Expr, A],
		min:Int,
		max:Int,
		delimiter:Parser[Expr, Unit],
		strategy:RepeatStrategy,
		isFirst:Boolean
	):Result[Expr, Pos, List[A]] = {
		(if (isFirst) {Success((), input, Set.empty[Expecting[Pos]], Cut.False)} else {delimiter.parse(input)}) match {
			case Failure(expectingDelimiter, cutDelimiter) => {
				if (min != 0 || cutDelimiter.toBoolean) {
					Failure(expectingDelimiter, cutDelimiter)
				} else {
					Success(Nil, input, expectingDelimiter, cutDelimiter)
				}
			}
			case successDelimiter:Success[Expr, Pos, Unit] => successDelimiter.flatMap[Expr, List[A]]({case Success1((), restDelimiter, expectingDelimiter, cutDelimiter) =>
				inner.parse(restDelimiter) match {
					case Failure(expectingA, cutA) => {
						if (min != 0 || cutDelimiter.toBoolean || cutA.toBoolean) {
							Failure(expectingDelimiter ++ expectingA, cutDelimiter | cutA)
						} else {
							Success(Nil, input, expectingDelimiter ++ expectingA, cutDelimiter | cutA)
						}
					}
					case successA:Success[Expr, Pos, A] => successA.flatMap[Expr, List[A]]({case Success1(valueA, restA, expectingA, cutA) =>
						if (max == 1 || restA == input) {
							// `restA == input` means quit if inner did not consume any input
							if (min != 0 || cutA.toBoolean || strategy == RepeatStrategy.Possessive) {
								Success(valueA :: Nil, restA, expectingDelimiter ++ expectingA, cutDelimiter | cutA)
							} else {
								if (strategy == RepeatStrategy.Greedy) {
									Success(
										Success1(valueA :: Nil, restA, expectingDelimiter ++ expectingA, cutDelimiter | cutA),
										List(
											Success1(Nil, input, expectingDelimiter, cutDelimiter)
										)
									)
								} else {
									Success(
										Success1(Nil, input, expectingDelimiter, cutDelimiter),
										List(
											Success1(valueA :: Nil, restA, expectingDelimiter ++ expectingA, cutDelimiter | cutA)
										)
									)
								}
							}
						} else {
							parse0(restA, inner, math.max(0, min - 1), max - 1, delimiter, strategy, false) match {
								case Failure(expectingC, cutC) => Failure(expectingA ++ expectingDelimiter ++ expectingC, cutA | cutDelimiter | cutC)
								case successC:Success[Expr, Pos, List[A]] => {
									val successCWithValA = successC.map({case Success1(valueC, restC, expectingC, cutC) =>
										Success1(valueA :: valueC, restC, expectingA ++ expectingDelimiter ++ expectingC, cutA | cutDelimiter | cutC)
									})
									if (min == 0 && !successCWithValA.choicesHead.isCut.toBoolean && strategy != RepeatStrategy.Possessive) {
										if (strategy == RepeatStrategy.Greedy) {
											successCWithValA :+
												Success1(Nil, input, Set.empty, Cut.False)
										} else {
											Success1(Nil, input, Set.empty[Expecting[Pos]], Cut.False) +: successCWithValA
										}
									} else {
										successCWithValA
									}
								}
							}
						}
					})
				}
			})
		}
	}
}
