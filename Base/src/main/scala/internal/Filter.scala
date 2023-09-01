package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Filter {
	def interpolator[Expr, A](
		backing:Interpolator[Expr, A],
		predicate:Function1[A, Boolean],
		predicateDescription:ExpectingDescription
	):Interpolator[Expr, A] = {
		new Interpolator[Expr, A] {
			def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
				backing.interpolate(input) match {
					case Success(choicesHead, choicesTail) => {
						val choices = choicesHead :: choicesTail
						val filteredChoices = choices.filter(x => predicate(x.value))
						filteredChoices match {
							case head :: tail => Success(head, tail)
							case Nil => {
								choices.map(s => Failure(s.expecting.mapDescriptions(_.where(predicateDescription)))).reduce[Failure[Pos]](_ or _)
							}
						}
					}
					case failure => failure
				}
			}
		}
	}
}
