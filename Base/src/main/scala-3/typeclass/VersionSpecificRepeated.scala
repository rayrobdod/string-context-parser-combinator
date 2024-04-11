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
		new BiRepeated[Expr, Unit, Unit] {
			type Acc = Unit
			override def init():Acc = ()
			override def append(acc:Acc, elem:Unit):Unit = {}
			override def result(acc:Acc):Unit = ()

			override def headTail:PartialExprFunction[Expr, Unit, (Unit, Unit)] = {
				PartialExprFunction[Expr, Unit, (Unit, Unit)](
					it => Expr(true),
					it => (it, it)
				)
			}
			override def isEmpty(it:Unit):Expr[Boolean] = Expr(true)
		}
	}

	given quotedToExprList[A](using Quotes, Type[A]):BiRepeated[Expr, Expr[A], Expr[List[A]]] = {
		new BiRepeated[Expr, Expr[A], Expr[List[A]]] {
			type Acc = Builder[Expr[A], List[Expr[A]]]
			override def init():Acc = List.newBuilder[Expr[A]]
			override def append(acc:Acc, elem:Expr[A]):Acc = {acc += elem}
			override def result(acc:Acc):Expr[List[A]] = Expr.ofList(acc.result())

			override def headTail:PartialExprFunction[Expr, Expr[List[A]], (Expr[A], Expr[List[A]])] = {
				PartialExprFunction[Expr, Expr[List[A]], (Expr[A], Expr[List[A]])](
					it => '{${it}.nonEmpty},
					it => ('{${it}.head}, '{${it}.tail}),
				)
			}
			override def isEmpty(it:Expr[List[A]]):Expr[Boolean] = {
				'{${it}.isEmpty}
			}
		}
	}
}

private[typeclass]
trait VersionSpecificLowPrioBiRepeated {
}
