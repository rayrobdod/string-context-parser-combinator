package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.collection.mutable.Builder
import scala.reflect.macros.blackbox.Context

private[typeclass]
trait VersionSpecificContraRepeated {
	trait ContraRepeateds[Expr[+_], Type[_]] {
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
	trait BiRepeateds[Expr[+_], Type[_]] {
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
				new BiRepeated[c.Expr, Unit, Unit] {
					type Acc = Unit
					override def init():Acc = ()
					override def append(acc:Acc, elem:Unit):Unit = {}
					override def result(acc:Acc):Unit = ()

					type Dec = Unit
					def contraInit(z:Unit):Unit = z
					override def headTail:PartialExprFunction[c.Expr, Unit, (Unit, Unit)] = {
						PartialExprFunction[c.Expr, Unit, (Unit, Unit)](
							_ => exprTrue,
							value => (value, value)
						)
					}
					override def isEmpty(it:Unit):c.Expr[Boolean] = exprTrue
				}
			}

			implicit override def toExprList[A](implicit typA:c.TypeTag[A]):BiRepeated[c.Expr, c.Expr[A], c.Expr[List[A]]] = {
				new BiRepeated[c.Expr, c.Expr[A], c.Expr[List[A]]] {
					type Acc = Builder[c.Tree, List[c.Tree]]
					override def init():Acc = List.newBuilder[c.Tree]
					override def append(acc:Acc, elem:c.Expr[A]):Acc = {acc += elem.tree}
					override def result(acc:Acc):c.Expr[List[A]] = {
						c.Expr[List[A]](
							c.universe.Apply(
								selectTermNames[Nothing]("_root_", "scala", "collection", "immutable", "List", "apply").tree,
								acc.result()
							)
						)
					}

					type Dec = c.Expr[List[A]]
					def contraInit(z:c.Expr[List[A]]):c.Expr[List[A]] = z
					override def headTail:PartialExprFunction[c.Expr, c.Expr[List[A]], (c.Expr[A], c.Expr[List[A]])] = {
						PartialExprFunction(
							it => select[List[A], Boolean](it, "nonEmpty"),
							it => (
								select[List[A], A](it, "head"),
								select[List[A], List[A]](it, "tail")
							)
						)
					}
					override def isEmpty(it:c.Expr[List[A]]):c.Expr[Boolean] = {
						select[List[A], Boolean](it, "isEmpty")
					}
				}
			}
		}
	}
}

private[typeclass]
trait VersionSpecificLowPrioBiRepeated {
}
