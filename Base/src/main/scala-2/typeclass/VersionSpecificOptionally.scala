package com.rayrobdod.stringContextParserCombinator
package typeclass

import scala.reflect.macros.blackbox.Context

private[typeclass]
trait VersionSpecificContraOptionally {
	trait ContraOptionallys[Expr[_], Type[_]] extends LowPrioContraOptionallys[Expr, Type] {
		implicit def unit:ContraOptionally[Expr, Unit, Unit]
	}
	trait LowPrioContraOptionallys[Expr[_], Type[_]] {
		implicit def toExprOption[A](implicit typA:Type[A]):ContraOptionally[Expr, Expr[A], Expr[Option[A]]]
	}

	def forContext(c:Context):ContraOptionallys[c.Expr, c.TypeTag] = {
		new ContraOptionallys[c.Expr, c.TypeTag] {
			private[this] val backing = BiOptionally.forContext(c)

			override def unit:ContraOptionally[c.Expr, Unit, Unit] = backing.unit
			override def toExprOption[A](implicit typA:c.TypeTag[A]):ContraOptionally[c.Expr, c.Expr[A], c.Expr[Option[A]]] = backing.toExprOption[A]
		}
	}
}

private[typeclass]
trait VersionSpecificLowPrioContraOptionally {
}

private[typeclass]
trait VersionSpecificBiOptionally {
	trait BiOptionallys[Expr[_], Type[_]] extends LowPrioBiOptionallys[Expr, Type] {
		implicit def unit:BiOptionally[Expr, Unit, Unit]
	}
	trait LowPrioBiOptionallys[Expr[_], Type[_]] {
		implicit def toExprOption[A](implicit typA:Type[A]):BiOptionally[Expr, Expr[A], Expr[Option[A]]]
	}

	def forContext(c:Context):BiOptionallys[c.Expr, c.TypeTag] = {
		new BiOptionallys[c.Expr, c.TypeTag] {
			private[this] val exprTrue = c.Expr[Boolean](c.universe.Literal(c.universe.Constant(true)))
			private[this] def select[A, Z](qualifier:c.Expr[A], name:String)(implicit typZ:c.TypeTag[Z]):c.Expr[Z] = {
				c.Expr[Z](c.universe.Select(qualifier.tree, c.universe.TermName(name)))
			}
			private[this] def selectTermNames[Z](root:String, names:String*)(implicit typZ:c.TypeTag[Z]):c.Expr[Z] = {
				val rootTree = c.universe.Ident(c.universe.TermName(root))
				val namesTree = names.foldLeft[c.universe.Tree](rootTree)({(folding, name) => c.universe.Select(folding, c.universe.TermName(name))})
				c.Expr[Z](namesTree)
			}

			override def unit:BiOptionally[c.Expr, Unit, Unit] = BiOptionally.apply(
				(),
				_ => (),
				_ => exprTrue,
				PartialExprFunction.identity(exprTrue)
			)

			override def toExprOption[A](implicit typA:c.TypeTag[A]):BiOptionally[c.Expr, c.Expr[A], c.Expr[Option[A]]] = BiOptionally.apply(
				selectTermNames[Option[A]]("_root_", "scala", "None"),
				value => {
					val rootTree = c.universe.Ident(c.universe.TermName("_root_"))
					val namesTree = List("scala", "Some", "apply").foldLeft[c.universe.Tree](rootTree)({(folding, name) => c.universe.Select(folding, c.universe.TermName(name))})
					c.Expr[Option[A]](c.universe.Apply(namesTree, List(value.tree)))
				},
				value => select[Option[A], Boolean](value, "isEmpty"),
				PartialExprFunction(
					value => select[Option[A], Boolean](value, "nonEmpty"),
					value => select[Option[A], A](value, "get")
				)
			)
		}
	}
}

private[typeclass]
trait VersionSpecificLowPrioBiOptionally {
}
