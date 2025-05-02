package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Filter {
	def interpolator[Ctx, Expr, A](
		backing:Interpolator[Ctx, Expr, A],
		predicate:(A, Ctx) => Boolean,
		predicateDescription:ExpectingDescription
	):Interpolator[Ctx, Expr, A] = {
		new Interpolator[Ctx, Expr, A] {
			override def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
				backing.interpolate(input) match {
					case Success(choicesHead, choicesTail) => {
						val choices = choicesHead :: choicesTail
						val filteredChoices = choices.filter(x => predicate(x.value, ctx))
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
