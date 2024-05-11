package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.collection.mutable.Builder
import scala.collection.mutable.StringBuilder
import scala.quoted.*

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
