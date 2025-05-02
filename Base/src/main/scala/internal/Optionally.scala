package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Optionally {
	def interpolator[Ctx, Expr, A, Z](
		backing:Interpolator[Ctx, Expr, A],
		strategy:RepeatStrategy,
		ev:typeclass.Optionally[Ctx, A, Z]
	):Interpolator[Ctx, Expr, Z] = {
		Repeat.interpolator(backing, 0, 1, new Pass[Ctx, Id, Id], strategy, new typeclass.Repeated[Ctx, A, Z] {
			type Acc = Z
			def init()(implicit ctx:Ctx):Acc = ev.none
			def append(acc:Acc, elem:A)(implicit ctx:Ctx):Acc = ev.some(elem)
			def result(acc:Acc)(implicit ctx:Ctx):Z = acc
		})
	}

	def extractor[Ctx, Expr[+_], Type[_], A, Z](
		backing:Extractor[Ctx, Expr, Type, A],
		strategy:RepeatStrategy,
		ev:typeclass.ContraOptionally[Ctx, Expr, A, Z]
	):Extractor[Ctx, Expr, Type, Z] = {
		val contraSomeFn = ev.contraSome
		val contraNoneFn = PartialExprFunction(
			(value:Z, ctx:Ctx) => ev.contraNone(value)(ctx),
			(_:Z, _:Ctx) => ()
		)

		strategy match {
			case RepeatStrategy.Possessive => {
				val ev2 = typeclass.ContraEithered[Ctx, Expr, A, Unit, Z](contraSomeFn, contraNoneFn)
				OrElse.extractor(backing, new Pass, ev2)
			}
			case RepeatStrategy.Greedy => {
				val ev2 = typeclass.ContraEithered[Ctx, Expr, A, Unit, Z](contraSomeFn, contraNoneFn)
				OrElse.extractor(Attempt.extractor(backing), new Pass, ev2)
			}
			case RepeatStrategy.Lazy => {
				val ev2 = typeclass.ContraEithered[Ctx, Expr, Unit, A, Z](contraNoneFn, contraSomeFn)
				OrElse.extractor(new Pass, backing, ev2)
			}
		}
	}

	def parser[Ctx, Expr[+_], Type[_], A, Z](
		backing:Parser[Ctx, Expr, Type, A],
		strategy:RepeatStrategy,
		ev:typeclass.BiOptionally[Ctx, Expr, A, Z]
	):Parser[Ctx, Expr, Type, Z] = {
		val someFn = (elem:A, ctx:Ctx) => ev.some(elem)(ctx)
		val noneFn = (_:Unit, ctx:Ctx) => ev.none(ctx)
		val contraSomeFn = ev.contraSome
		val contraNoneFn = PartialExprFunction(
			(value:Z, ctx:Ctx) => ev.contraNone(value)(ctx),
			(_:Z, _:Ctx) => ()
		)

		strategy match {
			case RepeatStrategy.Possessive => {
				val ev2 = typeclass.BiEithered[Ctx, Expr, A, Unit, Z](someFn, noneFn, contraSomeFn, contraNoneFn)
				OrElse.parser(backing, new Pass, ev2)
			}
			case RepeatStrategy.Greedy => {
				val ev2 = typeclass.BiEithered[Ctx, Expr, A, Unit, Z](someFn, noneFn, contraSomeFn, contraNoneFn)
				OrElse.parser(Attempt.parser(backing), new Pass, ev2)
			}
			case RepeatStrategy.Lazy => {
				val ev2 = typeclass.BiEithered[Ctx, Expr, Unit, A, Z](noneFn, someFn, contraNoneFn, contraSomeFn)
				OrElse.parser(new Pass, backing, ev2)
			}
		}
	}
}
