package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.collection.mutable.Builder
import scala.quoted.*

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

			type Dec = Unit
			def contraInit(z:Unit):Unit = z
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

			type Dec = Expr[List[A]]
			def contraInit(z:Expr[List[A]]):Expr[List[A]] = z
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
