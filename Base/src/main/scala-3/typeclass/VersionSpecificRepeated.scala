package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.collection.mutable.Builder
import scala.collection.mutable.StringBuilder
import scala.quoted.*
import Repeated.SplicePiece

private[typeclass]
trait VersionSpecificRepeated {
	/**
	 * Creates an Expr[String] consisting of the concatenation of the component Expr[String]s
	 * @since 0.1.1
	 */
	def quotedConcatenateString(using Quotes):Repeated[Expr[String], Expr[String]] = {
		final class ConcatenateString extends Repeated[Expr[String], Expr[String]] {
			sealed trait Acc
			object AccZero extends Acc
			final class AccOne(val elem: Expr[String]) extends Acc
			final class AccMany extends Acc {
				val builder: Builder[Expr[String], Expr[String]] = List.newBuilder.mapResult(parts =>
					'{
						${
							parts.foldLeft
								('{new scala.collection.mutable.StringBuilder})
								({(builder, part) => '{$builder.append($part)}})
						}
							.result
					}
				)
			}

			def init():Acc = AccZero
			def append(acc:Acc, elem:Expr[String]):Acc = acc match {
				case AccZero => new AccOne(elem)
				case accOne: AccOne => {
					val retval = new AccMany()
					retval.builder += accOne.elem
					retval.builder += elem
					retval
				}
				case accMany: AccMany => {
					accMany.builder += elem
					accMany
				}
			}
			def result(acc:Acc):Expr[String] = {
				acc match {
					case AccZero => Expr[String]("")
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
	 *     If None, `'{$newAccumulator.result()}` will be used.
	 *     If Some, should be equivalent to but more efficient than the None expr.
	 * @param ifOne the Expr to use when combining one scalar item together.
	 *     If None, `'{$newAccumulator.addOne($item).result()}` will be used.
	 *     If Some, should be equivalent to but more efficient than the None expr.
	 * @version 0.1.1
	 */
	def quotedFromSplicesUsingBuilder[A, Z](
			newAccumulator: Expr[Builder[A, Z]],
			ifZero: Option[() => Expr[Z]] = None,
			ifOne: Option[(Expr[A]) => Expr[Z]] = None,
			)(using Quotes, Type[A], Type[Z], Type[Builder[A, Z]],
	): Repeated[SplicePiece[Expr, A], Expr[Z]] = {
		final class FromSplicesUsingBuilder extends Repeated[SplicePiece[Expr, A], Expr[Z]] {
			sealed trait Acc
			object AccZero extends Acc
			final class AccOne(val elem: Expr[A]) extends Acc
			final class AccMany extends Acc {
				val builder: Builder[Expr[Builder[A, Z]] => Expr[_], Expr[Z]] = List.newBuilder.mapResult(parts =>
					'{
						val accumulator: Builder[A, Z] = ${newAccumulator}
						${Expr.block(parts.map(part => part('accumulator)), '{()})}
						accumulator.result()
					}
				)
			}

			def init():Acc = AccZero
			def append(acc:Acc, elem:SplicePiece[Expr, A]):Acc = acc match {
				case AccZero =>
					elem match {
						case _: SplicePiece.Zero[Expr] => AccZero
						case elemOne: SplicePiece.One[Expr, A] => new AccOne(elemOne.elem)
						case elemMany: SplicePiece.Many[Expr, A] => {
							val retval = new AccMany()
							retval.builder += {(acc) => '{$acc.addAll(${elemMany.iter})}}
							retval
						}
					}
				case accOne: AccOne => {
					elem match {
						case _: SplicePiece.Zero[Expr] => accOne
						case elemOne: SplicePiece.One[Expr, A] =>
							val retval = new AccMany()
							retval.builder += {(acc) => '{$acc.addOne(${accOne.elem})}}
							retval.builder += {(acc) => '{$acc.addOne(${elemOne.elem})}}
							retval
						case elemMany: SplicePiece.Many[Expr, A] =>
							val retval = new AccMany()
							retval.builder += {(acc) => '{$acc.addOne(${accOne.elem})}}
							retval.builder += {(acc) => '{$acc.addAll(${elemMany.iter})}}
							retval
					}
				}
				case accMany: AccMany => {
					elem match {
						case _: SplicePiece.Zero[Expr] =>
							// do nothing
						case elemOne: SplicePiece.One[Expr, A] =>
							accMany.builder += {(acc) => '{$acc.addOne(${elemOne.elem})}}
						case elemMany: SplicePiece.Many[Expr, A] =>
							accMany.builder += {(acc) => '{$acc.addAll(${elemMany.iter})}}
					}
					accMany
				}
			}

			def result(acc:Acc):Expr[Z] = {
				acc match {
					case AccZero => ifZero.map(_.apply()).getOrElse('{$newAccumulator.result()})
					case accOne: AccOne => ifOne.map(_.apply(accOne.elem)).getOrElse('{$newAccumulator.addOne(${accOne.elem}).result()})
					case accMany: AccMany => accMany.builder.result()
				}
			}
		}
		new FromSplicesUsingBuilder()
	}

	/**
	 * Splice a sequence of `SplicePiece`s together into a `List`
	 * @since 0.1.1
	 */
	implicit def quotedFromSplicesToExprList[A](using Quotes, Type[A]): Repeated[SplicePiece[Expr, A], Expr[List[A]]] =
		quotedFromSplicesUsingBuilder[A, List[A]](
			'{ List.newBuilder },
			Option(() => '{ List.empty }),
			Option((a: Expr[A]) => '{ List($a) }),
		)
}

private[typeclass]
trait VersionSpecificContraRepeated {
	given quotedUnit(using Quotes):ContraRepeated[Expr, Unit, Unit] = BiRepeated.quotedUnit
	given quotedToExprList[A](using Quotes, Type[A]):ContraRepeated[Expr, Expr[A], Expr[List[A]]] = BiRepeated.quotedToExprList
}

private[typeclass]
trait VersionSpecificLowPrioContraRepeated {
}

private[typeclass]
trait VersionSpecificBiRepeated {
	given quotedUnit(using Quotes):BiRepeated[Expr, Unit, Unit] = {
		BiRepeated.apply[Expr, Unit, Unit, Unit](
			() => (),
			(acc, _) => acc,
			(acc) => acc,
			PartialExprFunction[Expr, Unit, (Unit, Unit)](
				_ => Expr(true),
				value => (value, value),
			),
			_ => Expr(true),
		)
	}

	given quotedToExprList[A](using Quotes, Type[A]):BiRepeated[Expr, Expr[A], Expr[List[A]]] = {
		BiRepeated.apply[Expr, Expr[A], Builder[Expr[A], List[Expr[A]]], Expr[List[A]]](
			() => List.newBuilder[Expr[A]],
			(acc, elem) => {acc += elem},
			(acc) => Expr.ofList(acc.result()),
			PartialExprFunction[Expr, Expr[List[A]], (Expr[A], Expr[List[A]])](
				it => '{${it}.nonEmpty},
				it => ('{${it}.head}, '{${it}.tail}),
			),
			it => '{${it}.isEmpty},
		)
	}
}

private[typeclass]
trait VersionSpecificLowPrioBiRepeated {
}
