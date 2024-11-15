package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.reflect.macros.blackbox.Context

private[typeclass] trait VersionSpecificEithered extends LowPrioEithered {
	implicit def unitUnit:Eithered[Unit, Unit, Unit] = symmetric[Unit]
	implicit def unitGeneric[B, Z](implicit ev:Optionally[B, Z]):Eithered[Unit, B, Z] = Eithered(_ => ev.none, ev.some _)
	implicit def genericUnit[A, Z](implicit ev:Optionally[A, Z]):Eithered[A, Unit, Z] = Eithered(ev.some _, _ => ev.none)

	/**
	 * @version 0.1.1
	 */
	trait Eithereds[Expr[+_]] {
		def splicePiece[A]: Eithered[Expr[A], Expr[Iterable[A]], Repeated.SplicePiece[Expr, A]]
	}
	/**
	 * @version 0.1.1
	 */
	def forContext(c:Context):Eithereds[c.Expr] = {
		new Eithereds[c.Expr] {
			def splicePiece[A]: Eithered[c.Expr[A], c.Expr[Iterable[A]], Repeated.SplicePiece[c.Expr, A]] =
				Eithered(new Repeated.SplicePiece.One(_), new Repeated.SplicePiece.Many(_))
		}
	}
}

private[typeclass] trait LowPrioEithered {
	implicit def symmetric[A]:Eithered[A, A, A] = Eithered(Predef.identity _, Predef.identity _)
}

private[typeclass] trait VersionSpecificContraEithered extends LowPrioContraEithered {
	trait ContraEithereds[Expr[_]] extends LowPrioContraEithereds[Expr] {
		implicit def unitUnit:ContraEithered[Expr, Unit, Unit, Unit]
	}
	private[typeclass]
	trait LowPrioContraEithereds[Expr[_]] {
		implicit def symmetric[A]:ContraEithered[Expr, A, A, A]
	}

	def forContext(c:Context):ContraEithereds[c.Expr] = {
		val backing = BiEithered.forContext(c)

		new ContraEithereds[c.Expr] {
			implicit override def unitUnit:ContraEithered[c.Expr, Unit, Unit, Unit] = backing.unitUnit
			implicit override def symmetric[A]:ContraEithered[c.Expr, A, A, A] = backing.symmetric[A]
		}
	}
}

private[typeclass] trait LowPrioContraEithered {
	implicit def idSymmetric[A]:ContraEithered[Id, A, A, A] = BiEithered.idSymmetric
}

private[typeclass] trait VersionSpecificBiEithered extends LowPrioBiEithered {
	trait BiEithereds[Expr[_]] extends LowPrioBiEithereds[Expr] {
		implicit def unitUnit:BiEithered[Expr, Unit, Unit, Unit]
	}
	private[typeclass]
	trait LowPrioBiEithereds[Expr[_]] {
		implicit def symmetric[A]:BiEithered[Expr, A, A, A]
	}

	def forContext(c:Context):BiEithereds[c.Expr] = {
		new BiEithereds[c.Expr] {
			override def unitUnit:BiEithered[c.Expr, Unit, Unit, Unit] = this.symmetric[Unit]

			implicit override def symmetric[A]:BiEithered[c.Expr, A, A, A] = {
				val exprTrue = c.Expr[Boolean](c.universe.Liftable.liftBoolean(true))

				BiEithered.apply[c.Expr, A, A, A](
					Predef.identity _,
					Predef.identity _,
					PartialExprFunction.identity(exprTrue),
					PartialExprFunction.identity(exprTrue)
				)
			}
		}
	}
}

private[typeclass] trait LowPrioBiEithered {
	implicit def idSymmetric[A]:BiEithered[Id, A, A, A] = {
		BiEithered.apply[Id, A, A, A](
			Predef.identity _,
			Predef.identity _,
			PartialExprFunction.identity[Id, A](true),
			PartialExprFunction.identity[Id, A](true)
		)
	}
}
