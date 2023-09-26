package name.rayrobdod.stringContextParserCombinator

/**
 * The data needed to create am Unapply
 */
private[stringContextParserCombinator]
final case class UnapplyExpr[+Expr[_], +Type[_], -A] (
	condition: A => Expr[Boolean],
	parts: List[UnapplyExpr.Part[Expr, Type, A, _]]
)

private[stringContextParserCombinator]
object UnapplyExpr {
	final case class Part[+Expr[_], +Type[_], -A, Z](typ: Type[Z], value: A => Expr[Z]) {
		def contramapValue[B](contrafn: B => A):Part[Expr, Type, B, Z] = new Part(typ, contrafn.andThen(value))
	}
}

private[stringContextParserCombinator]
trait UnapplyExprs[Expr[_], Type[_]] {
	def empty:UnapplyExpr[Expr, Type, Any]

	def isChar(expecting:Char):UnapplyExpr[Expr, Type, Char]

	def isCodePoint(expecting:CodePoint):UnapplyExpr[Expr, Type, CodePoint]

	def isEqualTo[A](other:Expr[A])(implicit typ:Type[A]):UnapplyExpr[Expr, Type, Expr[A]]

	def ofType[Z](implicit typ:Type[Z]):UnapplyExpr[Expr, Type, Expr[Z]]

	def contramap[A, Z](
		backing: UnapplyExpr[Expr, Type, A],
		mapping:Z => A
	):UnapplyExpr[Expr, Type, Z]

	def widenWith[A, Z](
		backing: UnapplyExpr[Expr, Type, A],
		mapping: PartialExprFunction[Expr, Z, A]
	):UnapplyExpr[Expr, Type, Z]

	def eitheredLeft[A, Z](
		leftUnapplyExpr:UnapplyExpr[Expr, Type, A],
		ev:typeclass.ContraEithered[Expr, A, _, Z]
	):UnapplyExpr[Expr, Type, Z]

	def eitheredRight[B, Z](
		rightUnapplyExpr:UnapplyExpr[Expr, Type, B],
		ev:typeclass.ContraEithered[Expr, _, B, Z]
	):UnapplyExpr[Expr, Type, Z]

	def sequenced[A, B, Z](
		leftUnapplyExpr:UnapplyExpr[Expr, Type, A],
		rightUnapplyExpr:UnapplyExpr[Expr, Type, B],
		ev:typeclass.ContraSequenced[A, B, Z]
	):UnapplyExpr[Expr, Type, Z]

	def repeated[A, Z](
		childUnapplyExprs:List[UnapplyExpr[Expr, Type, A]],
		separator:typeclass.ContraRepeated[Expr, A, Z]
	):UnapplyExpr[Expr, Type, Z]

	def optionallySome[A, Z](
		childUnapplyExpr:UnapplyExpr[Expr, Type, A],
		ev:typeclass.ContraOptionally[Expr, A, Z]
	):UnapplyExpr[Expr, Type, Z]

	def optionallyNone[A, Z](
		ev:typeclass.ContraOptionally[Expr, A, Z]
	):UnapplyExpr[Expr, Type, Z]
}

private[stringContextParserCombinator]
object UnapplyExprs extends VersionSpecificUnapplyExprs {
	private[stringContextParserCombinator]
	abstract class Common[Expr[_], Type[_]](
		`true`: Expr[Boolean],
		`false`: Expr[Boolean],
		andBooleans: (Expr[Boolean], () => Expr[Boolean]) => Expr[Boolean]
	) extends UnapplyExprs[Expr, Type] {
		override def empty:UnapplyExpr[Expr, Type, Any] = {
			UnapplyExpr[Expr, Type, Any]((_:Any) => `true`, Nil)
		}

		def isChar(expecting:Char):UnapplyExpr[Expr, Type, Char] = {
			UnapplyExpr[Expr, Type, Char](
				{(scrutinee:Char) => if (expecting == scrutinee) {`true`} else {`false`}},
				Nil
			)
		}

		def isCodePoint(expecting:CodePoint):UnapplyExpr[Expr, Type, CodePoint] = {
			UnapplyExpr[Expr, Type, CodePoint](
				{(scrutinee:CodePoint) => if (expecting == scrutinee) {`true`} else {`false`}},
				Nil
			)
		}

		override def contramap[A, Z](
			backing: UnapplyExpr[Expr, Type, A],
			mapping: Z => A
		):UnapplyExpr[Expr, Type, Z] = {
			UnapplyExpr[Expr, Type, Z](
				mapping.andThen(backing.condition),
				backing.parts.map({part => part.contramapValue(mapping)})
			)
		}

		override def widenWith[A, Z](
			backing: UnapplyExpr[Expr, Type, A],
			mapping: PartialExprFunction[Expr, Z, A]
		):UnapplyExpr[Expr, Type, Z] = {
			UnapplyExpr[Expr, Type, Z](
				{(value:Z) =>
					val newCondition = mapping.isDefinedAt(value)
					val backingCondition = {() => backing.condition(mapping(value))}
					andBooleans(newCondition, backingCondition)
				},
				backing.parts.map({part => part.contramapValue(mapping.apply _)})
			)
		}

		override def eitheredLeft[A, Z](
			leftUnapplyExpr:UnapplyExpr[Expr, Type, A],
			ev:typeclass.ContraEithered[Expr, A, _, Z]
		):UnapplyExpr[Expr, Type, Z] = {
			this.widenWith(
				leftUnapplyExpr,
				ev.contraLeft
			)
		}

		override def eitheredRight[B, Z](
			rightUnapplyExpr:UnapplyExpr[Expr, Type, B],
			ev:typeclass.ContraEithered[Expr, _, B, Z]
		):UnapplyExpr[Expr, Type, Z] = {
			this.widenWith(
				rightUnapplyExpr,
				ev.contraRight
			)
		}

		override def sequenced[A, B, Z](
			leftUnapplyExpr:UnapplyExpr[Expr, Type, A],
			rightUnapplyExpr:UnapplyExpr[Expr, Type, B],
			ev:typeclass.ContraSequenced[A, B, Z]
		):UnapplyExpr[Expr, Type, Z] = {
			UnapplyExpr[Expr, Type, Z](
				{(value:Z) =>
					val (leftValue, rightValue) = ev.separate(value)

					val leftCondition = leftUnapplyExpr.condition(leftValue)
					val rightCondition = rightUnapplyExpr.condition(rightValue)

					andBooleans(leftCondition, () => rightCondition)
				},
				leftUnapplyExpr.parts.map({part => part.contramapValue((ev.separate _).andThen(_._1))}) :::
						rightUnapplyExpr.parts.map({part => part.contramapValue((ev.separate _).andThen(_._2))})
			)
		}

		override def repeated[A, Z](
			childUnapplyExprs:List[UnapplyExpr[Expr, Type, A]],
			separator:typeclass.ContraRepeated[Expr, A, Z]
		):UnapplyExpr[Expr, Type, Z] = {
			UnapplyExpr[Expr, Type, Z](
				{(value:Z) =>
					val (foldingCondition, foldingValueFn) = {
						childUnapplyExprs.foldLeft[(Expr[Boolean], () => separator.Dec)](
							(`true`, () => separator.contraInit(value))
						)({(folding, childUnapplyExpr) =>
							val (foldingCondition, foldingValueFn) = folding

							val stopCondition = {() => separator.headTail.isDefinedAt(foldingValueFn())}

							val childAndNewValue = {() => separator.headTail.apply(foldingValueFn())}
							val childValue = {() => childAndNewValue()._1}
							val newValue = {() => childAndNewValue()._2}

							val childCondition = {() => childUnapplyExpr.condition(childValue())}
							val newCondition = andBooleans(foldingCondition, {() => andBooleans(stopCondition(), childCondition)})

							(newCondition, newValue)
						})
					}
					andBooleans(foldingCondition, () => separator.isEmpty(foldingValueFn()))
				},
				{
					childUnapplyExprs.foldLeft[(List[UnapplyExpr.Part[Expr, Type, separator.Dec, _]], separator.Dec => separator.Dec)](
						(Nil, {z => z})
					)({(folding, childUnapplyExpr) =>
						val (previousParts, previousList) = folding
						val nextList:separator.Dec => separator.Dec = {(z:separator.Dec) => separator.headTail(previousList(z))._2}
						val childValue:separator.Dec => A = {(z:separator.Dec) => separator.headTail(previousList(z))._1}

						val childParts = childUnapplyExpr.parts.map({part => part.contramapValue(childValue)})

						(previousParts ::: childParts, nextList)
					})._1.map(parts => parts.contramapValue({(z:Z) => separator.contraInit(z)}))
				}
			)
		}

		override def optionallySome[A, Z](
			childUnapplyExpr:UnapplyExpr[Expr, Type, A],
			ev:typeclass.ContraOptionally[Expr, A, Z]
		):UnapplyExpr[Expr, Type, Z] = {
			this.widenWith(childUnapplyExpr, ev.contraSome)
		}

		override def optionallyNone[A, Z](
			ev:typeclass.ContraOptionally[Expr, A, Z]
		):UnapplyExpr[Expr, Type, Z] = {
			UnapplyExpr[Expr, Type, Z](ev.contraNone _, Nil)
		}
	}

	def forId: UnapplyExprs[Id, Class] = {
		val andBooleans = {(lhs:Boolean, rhs:() => Boolean) => lhs && rhs()}

		new UnapplyExprs.Common[Id, Class](true, false, andBooleans) {
			override def ofType[Z](implicit typ:Class[Z]):UnapplyExpr[Id, Class, Id[Z]] = {
				UnapplyExpr[Id, Class, Id[Z]]({(_:Id[Z]) => true}, UnapplyExpr.Part(typ, {(value:Id[Z]) => value}) :: Nil)
			}

			override def isEqualTo[A](other:Id[A])(implicit typ:Class[A]):UnapplyExpr[Id, Class, Id[A]] = {
				UnapplyExpr[Id, Class, Id[A]]({(value:Id[A]) => value == other}, Nil)
			}
		}
	}
}
