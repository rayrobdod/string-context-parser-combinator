package name.rayrobdod.stringContextParserCombinator

/**
 * The data needed to create an Unapply
 */
private[stringContextParserCombinator]
sealed trait UnapplyExpr[+Expr[_], +Type[_], -A] {
}

private[stringContextParserCombinator]
object UnapplyExpr {
	final case class Part[+Expr[_], +Type[_], -A, Z](typ: Type[Z], value: A => Expr[Z]) {
		def contramapValue[B](contrafn: B => A):Part[Expr, Type, B, Z] = new Part(typ, contrafn.andThen(value))
	}

	trait Primitives[Expr[_], Type[_]] {
		def exprTrue: Expr[Boolean]
		def exprFalse: Expr[Boolean]
		def andBooleanExprs(left: Expr[Boolean], rightFn: () => Expr[Boolean]): Expr[Boolean]

		def isEqualTo[A](left: Expr[A], right: Expr[A]): Expr[Boolean]
	}


	object Empty extends UnapplyExpr[Nothing, Nothing, Any] {
	}

	final case class IsEqualTo[Expr[_], Type[_], Z](
		other:Expr[Z],
		typ:Type[Z],
	) extends UnapplyExpr[Expr, Type, Expr[Z]] {
		type Z2 = Z
	}

	final case class OfType[Expr[_], Type[_], Z](
		typ:Type[Z]
	) extends UnapplyExpr[Expr, Type, Expr[Z]] {
	}

	final case class Contramap[Expr[_], Type[_], A, Z](
		backing: UnapplyExpr[Expr, Type, A],
		mapping:Z => A
	) extends UnapplyExpr[Expr, Type, Z] {
	}

	final case class WidenWith[Expr[_], Type[_], A, Z](
		backing: UnapplyExpr[Expr, Type, A],
		mapping: PartialExprFunction[Expr, Z, A]
	) extends UnapplyExpr[Expr, Type, Z] {
	}

	def EitheredLeft[Expr[_], Type[_], A, Z](
		leftUnapplyExpr:UnapplyExpr[Expr, Type, A],
		ev:typeclass.ContraEithered[Expr, A, _, Z]
	):UnapplyExpr[Expr, Type, Z] = {
		UnapplyExpr.WidenWith(
			leftUnapplyExpr,
			ev.contraLeft,
		)
	}

	def EitheredRight[Expr[_], Type[_], B, Z](
		rightUnapplyExpr:UnapplyExpr[Expr, Type, B],
		ev:typeclass.ContraEithered[Expr, _, B, Z]
	):UnapplyExpr[Expr, Type, Z] = {
		UnapplyExpr.WidenWith(
			rightUnapplyExpr,
			ev.contraRight,
		)
	}

	final case class Sequenced[Expr[_], Type[_], A, B, Z](
		leftUnapplyExpr:UnapplyExpr[Expr, Type, A],
		rightUnapplyExpr:UnapplyExpr[Expr, Type, B],
		ev:typeclass.ContraSequenced[A, B, Z]
	) extends UnapplyExpr[Expr, Type, Z] {
	}

	final case class Repeated[+Expr[_], Type[_], A, Z](
		childUnapplyExprs:List[UnapplyExpr[Expr, Type, A]],
		ev:typeclass.ContraRepeated[Expr, A, Z]
	) extends UnapplyExpr[Expr, Type, Z] {
		type A2 = A
	}

	def OptionallySome[Expr[_], Type[_], A, Z](
		childUnapplyExpr:UnapplyExpr[Expr, Type, A],
		ev:typeclass.ContraOptionally[Expr, A, Z]
	):UnapplyExpr[Expr, Type, Z] = {
		UnapplyExpr.WidenWith(
			childUnapplyExpr,
			ev.contraSome,
		)
	}

	final case class OptionallyNone[Expr[_], Type[_], A, Z](
		ev:typeclass.ContraOptionally[Expr, A, Z]
	) extends UnapplyExpr[Expr, Type, Z] {
	}
}
