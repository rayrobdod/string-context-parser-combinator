package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object DelayedConstruction {
	def interpolator[Expr, A](
		backing:() => name.rayrobdod.stringContextParserCombinator.Interpolator[Expr, A]
	):Interpolator[Expr, A] = {
		new Interpolator[Expr, A] {
			def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
				backing.apply().impl.interpolate(input)
			}
		}
	}

	def extractor[Expr[+_], Type[_], A](
		backing:() => Extractor[Expr, Type, A]
	):Extractor[Expr, Type, A] = {
		new Extractor[Expr, Type, A] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos]):Result[Unit, Pos, UnapplyExpr[Expr, Type, A]] = {
				backing.apply().extractor(input)
			}
		}
	}

	def parser[Expr[+_], Type[_], A](
		backing:() => Parser[Expr, Type, A]
	):Parser[Expr, Type, A] = {
		new Parser[Expr, Type, A] {
			override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
				backing.apply().interpolate(input)
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos]):Result[Unit, Pos, UnapplyExpr[Expr, Type, A]] = {
				backing.apply().extractor(input)
			}
		}
	}
}
