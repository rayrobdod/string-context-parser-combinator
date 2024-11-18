package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.collection.mutable.Builder
import scala.reflect.macros.blackbox.Context
import Repeated.SplicePiece

private[typeclass]
trait VersionSpecificRepeated {
	/**
	 * Creates an Expr[String] consisting of the concatenation of the component Expr[String]s
	 * @since 0.1.1
	 */
	def forContextConcatenateString(c:Context):Repeated[c.Expr[String], c.Expr[String]] = {
		import c.universe.Tree
		import c.universe.Quasiquote
		val ttString0 = c.universe.typeTag[String]
		final class ConcatenateString extends Repeated[c.Expr[String], c.Expr[String]] {
			val accumulatorName = c.freshName(c.universe.TermName("accumulator$"))
			val accumulatorTypeTree = c.universe.TypeTree(
				c.universe.rootMirror.staticClass("scala.collection.mutable.StringBuilder").asType.toTypeConstructor
			)
			val accumulatorIdent = c.universe.Ident(accumulatorName)
			val accumulatorValDef = c.universe.ValDef(
				c.universe.NoMods,
				accumulatorName,
				accumulatorTypeTree,
				q"new $accumulatorTypeTree()",
			)

			implicit val ttString: c.TypeTag[String] = ttString0

			sealed trait Acc
			final object AccZero extends Acc
			final class AccOne(val elem: c.Expr[String]) extends Acc
			final class AccMany extends Acc {
				val builder: Builder[Tree, c.Expr[String]] = List.newBuilder.mapResult(stat =>
					c.Expr[String](
						c.universe.Block(
							stat,
							q"$accumulatorIdent.toString"
						)
					)
				)
			}

			def init():Acc = AccZero
			def append(acc:Acc, elem:c.Expr[String]):Acc = acc match {
				case AccZero => new AccOne(elem)
				case accOne: AccOne => {
					val retval = new AccMany()
					retval.builder += accumulatorValDef
					retval.builder += q"$accumulatorIdent.append(${accOne.elem})"
					retval.builder += q"$accumulatorIdent.append($elem)"
					retval
				}
				case accMany: AccMany => {
					accMany.builder += q"$accumulatorIdent.append($elem)"
					accMany
				}
			}
			def result(acc:Acc):c.Expr[String] = {
				acc match {
					case AccZero => c.Expr[String](c.universe.Literal(c.universe.Constant("")))
					case accOne: AccOne => accOne.elem
					case accMany: AccMany => accMany.builder.result()
				}
			}
		}
		new ConcatenateString()
	}

	/**
	 * Splice a sequence of `SplicePiece`s together using a Builder
	 * @param newAccumulator an Expr creating a new Builder
	 * @param ifZero the Expr to use when combining zero items together.
	 *     If None, `newAccumulator.result()` will be used.
	 *     If Some, should ideally be equivalent to but more efficient than the None expr.
	 * @param ifOneScalar the Expr to use when combining one scalar item together.
	 *     When not defined, `newAccumulator.+=(item).result()` will be used.
	 *     The definition should be equivalent to but more efficient than the undefined expr.
	 * @param ifOneSplice the Expr to use when combining one splice item together.
	 *     When not defined, `newAccumulator.++=(item).result()` will be used.
	 *     The definition should be equivalent to but more efficient than the undefined expr.
	 * @version 0.1.1
	 */
	def forContextFromSplicesUsingBuilder[A, Z](c:Context)(
			newAccumulator: c.Expr[Builder[A, Z]],
			ifZero: Option[() => c.Expr[Z]],
			ifOneScalar: PartialFunction[c.Expr[A], c.Expr[Z]],
			ifOneSplice: PartialFunction[c.Expr[Iterable[A]], c.Expr[Z]],
			)(implicit
			accumulatorType: c.universe.TypeTag[Builder[A, Z]],
			zType: c.universe.TypeTag[Z],
	): Repeated[SplicePiece[c.Expr, A], c.Expr[Z]] = {
		// using default arguments confuses the typechecker (found `c.Expr` required `x$1.Expr`), so don't provide default arguments

		import c.universe.Tree
		import c.universe.Quasiquote

		final class FromSplicesUsingBuilder extends Repeated[SplicePiece[c.Expr, A], c.Expr[Z]] {
			val accumulatorName = c.freshName(c.universe.TermName("accumulator$"))
			val accumulatorTypeTree = c.universe.TypeTree(accumulatorType.tpe)
			val accumulatorIdent = c.universe.Ident(accumulatorName)
			val accumulatorExpr = c.Expr(accumulatorIdent)(accumulatorType)

			val accumulatorValDef = c.universe.ValDef(
				c.universe.NoMods,
				accumulatorName,
				accumulatorTypeTree,
				newAccumulator.tree
			)

			sealed trait Acc
			final object AccZero extends Acc
			final class AccOneScalar(val elem: c.Expr[A]) extends Acc
			final class AccOneSplice(val iter: c.Expr[Iterable[A]]) extends Acc
			final class AccMany extends Acc {
				val builder: Builder[Tree, c.Expr[Z]] = List.newBuilder.mapResult(stat =>
					c.Expr[Z](
						c.universe.Block(
							stat,
							q"$accumulatorIdent.result()"
						)
					)
				)
			}

			def init():Acc = AccZero
			def append(acc:Acc, elem:SplicePiece[c.Expr, A]):Acc = acc match {
				case AccZero =>
					elem match {
						case _: SplicePiece.Zero[c.Expr] => AccZero
						case elemOne: SplicePiece.One[c.Expr, A] => new AccOneScalar(elemOne.elem)
						case elemMany: SplicePiece.Many[c.Expr, A] => new AccOneSplice(elemMany.iter)
					}
				case accOne: AccOneScalar => {
					elem match {
						case _: SplicePiece.Zero[c.Expr] => accOne
						case elemOne: SplicePiece.One[c.Expr, A] =>
							val retval = new AccMany()
							retval.builder += accumulatorValDef
							retval.builder += q"$accumulatorIdent.+=(${accOne.elem})"
							retval.builder += q"$accumulatorIdent.+=(${elemOne.elem})"
							retval
						case elemMany: SplicePiece.Many[c.Expr, A] =>
							val retval = new AccMany()
							retval.builder += accumulatorValDef
							retval.builder += q"$accumulatorIdent.+=(${accOne.elem})"
							retval.builder += q"$accumulatorIdent.++=(${elemMany.iter})"
							retval
					}
				}
				case accOne: AccOneSplice => {
					elem match {
						case _: SplicePiece.Zero[c.Expr] => accOne
						case elemOne: SplicePiece.One[c.Expr, A] =>
							val retval = new AccMany()
							retval.builder += accumulatorValDef
							retval.builder += q"$accumulatorIdent.++=(${accOne.iter})"
							retval.builder += q"$accumulatorIdent.+=(${elemOne.elem})"
							retval
						case elemMany: SplicePiece.Many[c.Expr, A] =>
							val retval = new AccMany()
							retval.builder += accumulatorValDef
							retval.builder += q"$accumulatorIdent.++=(${accOne.iter})"
							retval.builder += q"$accumulatorIdent.++=(${elemMany.iter})"
							retval
					}
				}
				case accMany: AccMany => {
					elem match {
						case _: SplicePiece.Zero[c.Expr] =>
							// do nothing
						case elemOne: SplicePiece.One[c.Expr, A] =>
							accMany.builder += q"$accumulatorIdent.+=(${elemOne.elem})"
						case elemMany: SplicePiece.Many[c.Expr, A] =>
							accMany.builder += q"$accumulatorIdent.++=(${elemMany.iter})"
					}
					accMany
				}
			}

			def result(acc:Acc):c.Expr[Z] = {
				acc match {
					case AccZero => ifZero.map(_.apply()).getOrElse(c.Expr[Z](q"$newAccumulator.result()"))
					case accOne: AccOneScalar => ifOneScalar.applyOrElse(accOne.elem, (e: c.Expr[A]) => c.Expr[Z](q"$newAccumulator.+=(${e}).result()"))
					case accOne: AccOneSplice => ifOneSplice.applyOrElse(accOne.iter, (es: c.Expr[Iterable[A]]) => c.Expr[Z](q"$newAccumulator.++=(${es}).result()"))
					case accMany: AccMany => accMany.builder.result()
				}
			}

		}
		new FromSplicesUsingBuilder()
	}

	/**
	 * Splice a sequence of `SplicePiece`s together into a `List`
	 * @version 0.1.1
	 */
	def forContextFromSplicesToExprList[A](c: Context)(implicit
			accumulatorType: c.universe.TypeTag[Builder[A, List[A]]],
			zType: c.universe.TypeTag[List[A]],
	): Repeated[SplicePiece[c.Expr, A], c.Expr[List[A]]] = {
		forContextFromSplicesUsingBuilder[A, List[A]](c)(
			c.universe.reify(List.newBuilder),
			Option(() => c.universe.reify(List.empty)),
			{case (a: c.Expr[A]) => c.universe.reify(List(a.splice))},
			{case (a: c.Expr[_]) if a.staticType <:< zType.tpe => c.Expr[List[A]](a.tree)},
		)
	}
}

private[typeclass]
trait VersionSpecificContraRepeated {
	trait ContraRepeateds[Expr[_], Type[_]] {
		implicit def unit:ContraRepeated[Expr, Unit, Unit]
		implicit def toExprList[A](implicit tt:Type[A]):ContraRepeated[Expr, Expr[A], Expr[List[A]]]
	}

	def forContext(c:Context):ContraRepeateds[c.Expr, c.TypeTag] = {
		new ContraRepeateds[c.Expr, c.TypeTag] {
			implicit override def unit:ContraRepeated[c.Expr, Unit, Unit] =
				BiRepeated.forContext(c).unit

			implicit override def toExprList[A](implicit tt:c.TypeTag[A]):ContraRepeated[c.Expr, c.Expr[A], c.Expr[List[A]]] =
				BiRepeated.forContext(c).toExprList[A]
		}
	}
}

private[typeclass]
trait VersionSpecificLowPrioContraRepeated {
}

private[typeclass]
trait VersionSpecificBiRepeated {
	trait BiRepeateds[Expr[_], Type[_]] {
		implicit def unit:BiRepeated[Expr, Unit, Unit]
		implicit def toExprList[A](implicit typA:Type[A]):BiRepeated[Expr, Expr[A], Expr[List[A]]]
	}

	def forContext(c:Context):BiRepeateds[c.Expr, c.TypeTag] = {
		new BiRepeateds[c.Expr, c.TypeTag] {
			private[this] val exprTrue = c.Expr[Boolean](c.universe.Literal(c.universe.Constant(true)))
			private[this] def select[A, Z](qualifier:c.Expr[A], name:String)(implicit typZ:c.TypeTag[Z]):c.Expr[Z] = {
				c.Expr[Z](c.universe.Select(qualifier.tree, c.universe.TermName(name)))
			}
			private[this] def selectTermNames[Z](root:String, names:String*)(implicit typZ:c.TypeTag[Z]):c.Expr[Z] = {
				val rootTree = c.universe.Ident(c.universe.TermName(root))
				val namesTree = names.foldLeft[c.universe.Tree](rootTree)({(folding, name) => c.universe.Select(folding, c.universe.TermName(name))})
				c.Expr[Z](namesTree)
			}

			implicit override def unit:BiRepeated[c.Expr, Unit, Unit] = {
				BiRepeated.apply[c.Expr, Unit, Unit, Unit](
					() => (),
					(acc, _) => acc,
					(acc) => acc,
					PartialExprFunction[c.Expr, Unit, (Unit, Unit)](
						_ => exprTrue,
						value => (value, value)
					),
					_ => exprTrue,
				)
			}

			implicit override def toExprList[A](implicit typA:c.TypeTag[A]):BiRepeated[c.Expr, c.Expr[A], c.Expr[List[A]]] = {
				BiRepeated.apply[c.Expr, c.Expr[A], Builder[c.Tree, List[c.Tree]], c.Expr[List[A]]](
					() => List.newBuilder[c.Tree],
					(acc, elem) => {acc += elem.tree},
					(acc) => {
						c.Expr[List[A]](
							c.universe.Apply(
								selectTermNames[Nothing]("_root_", "scala", "collection", "immutable", "List", "apply").tree,
								acc.result()
							)
						)
					},
					PartialExprFunction(
						it => select[List[A], Boolean](it, "nonEmpty"),
						it => (
							select[List[A], A](it, "head"),
							select[List[A], List[A]](it, "tail")
						)
					),
					it => select[List[A], Boolean](it, "isEmpty"),
				)
			}
		}
	}
}

private[typeclass]
trait VersionSpecificLowPrioBiRepeated {
}
