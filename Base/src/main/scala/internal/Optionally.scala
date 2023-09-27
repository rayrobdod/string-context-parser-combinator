package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Optionally {
	def interpolator[Expr, A, Z](
		backing:Interpolator[Expr, A],
		strategy:RepeatStrategy,
		ev:typeclass.Optionally[A, Z]
	):Interpolator[Expr, Z] = {
		Repeat.interpolator(backing, 0, 1, new Pass[Id, Id], strategy, new typeclass.Repeated[A, Z] {
			type Acc = Z
			def init():Acc = ev.none
			def append(acc:Acc, elem:A):Acc = ev.some(elem)
			def result(acc:Acc):Z = acc
		})
	}

	def extractor[Expr[+_], Type[_], A, Z](
		backing:Extractor[Expr, Type, A],
		strategy:RepeatStrategy,
		ev:typeclass.ContraOptionally[Expr, A, Z]
	):Extractor[Expr, Type, Z] = {
		val contraSomeFn = ev.contraSome
		val contraNoneFn = PartialExprFunction(
			ev.contraNone _,
			(_:Z) => ()
		)

		strategy match {
			case RepeatStrategy.Possessive => {
				val ev2 = typeclass.ContraEithered[Expr, A, Unit, Z](contraSomeFn, contraNoneFn)
				OrElse.extractor(backing, new Pass, ev2)
			}
			case RepeatStrategy.Greedy => {
				val ev2 = typeclass.ContraEithered[Expr, A, Unit, Z](contraSomeFn, contraNoneFn)
				OrElse.extractor(Attempt.extractor(backing), new Pass, ev2)
			}
			case RepeatStrategy.Lazy => {
				val ev2 = typeclass.ContraEithered[Expr, Unit, A, Z](contraNoneFn, contraSomeFn)
				OrElse.extractor(new Pass, backing, ev2)
			}
		}
	}

	def parser[Expr[+_], Type[_], A, Z](
		backing:Parser[Expr, Type, A],
		strategy:RepeatStrategy,
		ev:typeclass.BiOptionally[Expr, A, Z]
	):Parser[Expr, Type, Z] = {
		val someFn = (elem:A) => ev.some(elem)
		val noneFn = (_:Unit) => ev.none
		val contraSomeFn = ev.contraSome
		val contraNoneFn = PartialExprFunction(
			ev.contraNone _,
			(_:Z) => ()
		)

		strategy match {
			case RepeatStrategy.Possessive => {
				val ev2 = typeclass.BiEithered[Expr, A, Unit, Z](someFn, noneFn, contraSomeFn, contraNoneFn)
				OrElse.parser(backing, new Pass, ev2)
			}
			case RepeatStrategy.Greedy => {
				val ev2 = typeclass.BiEithered[Expr, A, Unit, Z](someFn, noneFn, contraSomeFn, contraNoneFn)
				OrElse.parser(Attempt.parser(backing), new Pass, ev2)
			}
			case RepeatStrategy.Lazy => {
				val ev2 = typeclass.BiEithered[Expr, Unit, A, Z](noneFn, someFn, contraNoneFn, contraSomeFn)
				OrElse.parser(new Pass, backing, ev2)
			}
		}
	}
}
