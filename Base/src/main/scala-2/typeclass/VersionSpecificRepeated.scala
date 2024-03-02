package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.collection.mutable.Builder
import scala.reflect.macros.blackbox.Context

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
				new BiRepeated[c.Expr, Unit, Unit] {
					type Acc = Unit
					override def init():Acc = ()
					override def append(acc:Acc, elem:Unit):Unit = {}
					override def result(acc:Acc):Unit = ()

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
