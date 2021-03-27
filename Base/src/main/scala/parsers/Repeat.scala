package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class Repeat[Expr, A, Z](
	inner:Parser[Expr, A],
	min:Int,
	max:Int,
	delimiter:Parser[Expr, Unit],
	ev:typelevel.Repeated[A, Z]
) extends AbstractParser[Expr, Z] {
	require(min >= 0)
	require(max >= 1)
	require(max >= min)

	def parse(input:Input[Expr]):Result[Expr, Z] = {
		var counter:Int = 0
		val accumulator = ev.init()
		var remaining:Input[Expr] = input
		var continue:Boolean = true
		var accumulatedExpecting:Set[Expecting] = Set.empty
		var accumulatedCut:Cut = Cut.False
		var lastCut:Cut = Cut.False
		var failedInInner:Boolean = false

		while (continue && counter < max) {
			if (counter != 0) {
				delimiter.parse(remaining) match {
					case Success((), r, t, c) => {
						accumulatedCut = accumulatedCut | c
						remaining = r
						accumulatedExpecting = (if (c.toBoolean) {Set.empty} else {accumulatedExpecting}) ++ t
					}
					case Failure(t, c) => {
						lastCut = c
						accumulatedCut = accumulatedCut | c
						accumulatedExpecting = (if (c.toBoolean) {Set.empty} else {accumulatedExpecting}) ++ t
						continue = false
					}
				}
			}
			if (continue) {
				inner.parse(remaining) match {
					case Success(a, r, t, c) => {
						counter += 1
						accumulatedCut = accumulatedCut | c
						ev.append(accumulator, a)
						continue = (remaining != r) // quit if inner seems to be making no progress
						remaining = r
						accumulatedExpecting = (if (c.toBoolean) {Set.empty} else {accumulatedExpecting}) ++ t
					}
					case Failure(t, c) => {
						failedInInner = true
						lastCut = c
						accumulatedCut = accumulatedCut | c
						accumulatedExpecting = (if (c.toBoolean) {Set.empty} else {accumulatedExpecting}) ++ t
						continue = false
					}
				}
			}
		}
		if (min <= counter && counter <= max && !(lastCut.toBoolean)) {
			val oneMoreRoundExpecting = if (failedInInner) {
				inner.parse(new Input[Nothing](List(("", remaining.position)), List.empty, x => x)) match {
					case Success(_, _, _, _) => Set.empty
					case Failure(t, _) => t
				}
			} else {
				Set.empty
			}
			Success(ev.result(accumulator), remaining, accumulatedExpecting ++ oneMoreRoundExpecting, accumulatedCut)
		} else {
			Failure(accumulatedExpecting, accumulatedCut)
		}
	}
}
