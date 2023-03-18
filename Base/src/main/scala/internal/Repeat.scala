package com.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
final class Repeat[Expr, A, Z](
	inner:Parser[Expr, A],
	min:Int,
	max:Int,
	delimiter:Parser[Expr, Unit],
	strategy:RepeatStrategy,
	ev:typeclass.Repeated[A, Z]
) extends Parser[Expr, Z] {
	require(min >= 0)
	require(max >= 1)
	require(max >= min)

	def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
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
	private def parse0[Expr, Pos : Ordering, A](
		input:Input[Expr, Pos],
		inner:Parser[Expr, A],
		min:Int,
		max:Int,
		delimiter:Parser[Expr, Unit],
		strategy:RepeatStrategy,
		isFirst:Boolean
	):Result[Expr, Pos, List[A]] = {
		(if (isFirst) {Success((), input, ExpectingSet.empty[Pos])} else {delimiter.interpolate(input)}) match {
			case failureDelimiter:Failure[Pos] => {
				if (min != 0 || failureDelimiter.isPositionGt(input.position)) {
					failureDelimiter
				} else {
					Success(Nil, input, failureDelimiter.expecting)
				}
			}
			case successDelimiter:Success[Expr, Pos, Unit] => successDelimiter.flatMap[Expr, List[A]]({case Success1((), restDelimiter, expectingDelimiter) =>
				inner.interpolate(restDelimiter) match {
					case failureA:Failure[Pos] => {
						if (min != 0 || failureA.isPositionGt(restDelimiter.position)) {
							failureA or expectingDelimiter
						} else {
							Success(Nil, input, expectingDelimiter ++ failureA.expecting)
						}
					}
					case successA:Success[Expr, Pos, A] => successA.flatMap[Expr, List[A]]({case Success1(valueA, restA, expectingA) =>
						if (max == 1 || restA == input) {
							// `restA == input` means quit if inner did not consume any input
							if (min != 0 || strategy == RepeatStrategy.Possessive) {
								Success(valueA :: Nil, restA, expectingDelimiter ++ expectingA)
							} else {
								if (strategy == RepeatStrategy.Greedy) {
									Success(
										Success1(valueA :: Nil, restA, expectingDelimiter ++ expectingA),
										List(
											Success1(Nil, input, expectingDelimiter)
										)
									)
								} else {
									Success(
										Success1(Nil, input, expectingDelimiter),
										List(
											Success1(valueA :: Nil, restA, expectingDelimiter ++ expectingA)
										)
									)
								}
							}
						} else {
							parse0(restA, inner, math.max(0, min - 1), max - 1, delimiter, strategy, false) match {
								case failureC:Failure[Pos] => failureC or (expectingA ++ expectingDelimiter)
								case successC:Success[Expr, Pos, List[A]] => {
									val successCWithValA = successC.map({case Success1(valueC, restC, expectingC) =>
										Success1(valueA :: valueC, restC, expectingA ++ expectingDelimiter ++ expectingC)
									})
									if (min == 0 && strategy != RepeatStrategy.Possessive) {
										if (strategy == RepeatStrategy.Greedy) {
											successCWithValA :+
												Success1(Nil, input, ExpectingSet.empty)
										} else {
											Success1(Nil, input, ExpectingSet.empty[Pos]) +: successCWithValA
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
