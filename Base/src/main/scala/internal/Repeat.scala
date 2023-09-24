package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Repeat {
	def interpolator[Expr, A, Z](
		inner:Interpolator[Expr, A],
		min:Int,
		max:Int,
		delimiter:Interpolator[Expr, Unit],
		strategy:RepeatStrategy,
		ev:typeclass.Repeated[A, Z]
	):Interpolator[Expr, Z] = {
		require(min >= 0)
		require(max >= 1)
		require(max >= min)

		new Interpolator[Expr, Z] {
			def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				Repeat.parse(
					{(x:Input[ExprZ, Pos]) => inner.interpolate(x)},
					min,
					max,
					{(x:Input[ExprZ, Pos]) => delimiter.interpolate(x)},
					strategy,
					true,
					input
				).mapValues({parts =>
					ev.result(parts.foldLeft(ev.init())((acc, part) => ev.append(acc, part)))
				})
			}
		}
	}

	def extractor[Expr[_], Type[_], A, Z](
		inner:Extractor[Expr, Type, A],
		min:Int,
		max:Int,
		delimiter:Extractor[Expr, Type, Unit],
		strategy:RepeatStrategy,
		ev:typeclass.ContraRepeated[Expr, A, Z]
	):Extractor[Expr, Type, Z] = {
		require(min >= 0)
		require(max >= 1)
		require(max >= min)

		new Extractor[Expr, Type, Z] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Expr, Type]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Z]] = {
				Repeat.parse(
					{(x:Input[Unit, Pos]) => inner.extractor(x)},
					min,
					max,
					{(x:Input[Unit, Pos]) => delimiter.extractor(x).mapValues(_ => ())},
					strategy,
					true,
					input
				).mapValues({parts => exprs.repeated(parts, ev)})
			}
		}
	}

	def parser[Expr[_], Type[_], A, Z](
		inner:Parser[Expr, Type, A],
		min:Int,
		max:Int,
		delimiter:Parser[Expr, Type, Unit],
		strategy:RepeatStrategy,
		ev:typeclass.BiRepeated[Expr, A, Z]
	):Parser[Expr, Type, Z] = {
		require(min >= 0)
		require(max >= 1)
		require(max >= min)

		new Parser[Expr, Type, Z] {
			override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				Repeat.parse(
					{(x:Input[ExprZ, Pos]) => inner.interpolate(x)},
					min,
					max,
					{(x:Input[ExprZ, Pos]) => delimiter.interpolate(x)},
					strategy,
					true,
					input
				).mapValues({parts =>
					val acc = ev.init()
					parts.foreach(part => ev.append(acc, part))
					ev.result(acc)
				})
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Expr, Type]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Z]] = {
				Repeat.parse(
					{(x:Input[Unit, Pos]) => inner.extractor(x)},
					min,
					max,
					{(x:Input[Unit, Pos]) => delimiter.extractor(x).mapValues(_ => ())},
					strategy,
					true,
					input
				).mapValues({parts => exprs.repeated(parts, ev)})
			}
		}
	}

	private def parse[Expr, Pos : Ordering, A](
		useInner: Input[Expr, Pos] => Result[Expr, Pos, A],
		min:Int,
		max:Int,
		useDelimiter: Input[Expr, Pos] => Result[Expr, Pos, Unit],
		strategy:RepeatStrategy,
		isFirst:Boolean,
		input:Input[Expr, Pos]
	):Result[Expr, Pos, List[A]] = {
		(if (isFirst) {Success((), input, ExpectingSet.empty[Pos])} else {useDelimiter(input)}) match {
			case failureDelimiter:Failure[Pos] => {
				if (min != 0 || failureDelimiter.isPositionGt(input.position)) {
					failureDelimiter
				} else {
					Success(Nil, input, failureDelimiter.expecting)
				}
			}
			case successDelimiter:Success[Expr, Pos, Unit] => successDelimiter.flatMap[Expr, List[A]]({case Success1((), restDelimiter, expectingDelimiter) =>
				useInner(restDelimiter) match {
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
							parse(useInner, math.max(0, min - 1), max - 1, useDelimiter, strategy, false, restA) match {
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
