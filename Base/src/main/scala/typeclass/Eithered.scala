package name.rayrobdod.stringContextParserCombinator
package typeclass

import com.eed3si9n.ifdef.ifdef

/**
 * Describes how to represent a result that may be one of two results
 *
 * Below is example of defining and using a custom Eithered.
 *
 * ```scala
 * import java.io.File
 * import java.net.URI
 * import java.util.UUID
 * import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._
 * import name.rayrobdod.stringContextParserCombinator.typeclass.Eithered
 *
 * given Eithered[File, UUID, URI] with {
 *   def left(f: File): URI = f.toURI
 *   def right(id: UUID): URI = new URI("urn", "uuid:" + id.toString, null)
 * }
 *
 * val uuidParser:Interpolator[UUID] = ofType[UUID]
 * val fileParser:Interpolator[File] = ofType[File]
 * val p:Interpolator[URI] = (fileParser:Interpolator[File]) orElse (uuidParser:Interpolator[UUID]) // using Eithered[File, UUID, URI]
 *
 * p.interpolate(StringContext("", ""), new File("/tmp") :: Nil) // `file:///tmp`: URI
 * p.interpolate(StringContext("", ""), UUID.randomUUID() :: Nil) // `urn:uuid:429bf7eb-650e-4f8c-be3f-1420913a6bd7`: URI
 * ```
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Interpolator.orElse Interpolator.orElse]]
 * @tparam A the first choice
 * @tparam B the second choice
 * @tparam Z the result container
 */
trait Eithered[-A, -B, +Z] {
	def left(elem:A):Z
	def right(elem:B):Z
}

/**
 * Describes how to disambiguate the union of two types
 *
 * The parser determines whether the left or right branch is taken.
 * The return value's `Expr[Boolean]` indicates whether the value matches the branch
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Extractor.orElse Extractor.orElse]]
 * @tparam A the first choice
 * @tparam B the second choice
 * @tparam Z the result container
 */
trait ContraEithered[+Expr[_], +A, +B, -Z] {
	def contraLeft:PartialExprFunction[Expr, Z, A]
	def contraRight:PartialExprFunction[Expr, Z, B]
}

/**
 * @see [[name.rayrobdod.stringContextParserCombinator.Parser.orElse Parser.orElse]]
 * @tparam A the first choice
 * @tparam B the second choice
 * @tparam Z the result container
 */
trait BiEithered[Expr[_], A, B, Z]
		extends Eithered[A, B, Z]
		with ContraEithered[Expr, A, B, Z]

/**
 * Predefined implicit implementations of Eithered
 * and methods to create new Eithereds
 */
object Eithered extends LowPrioEithered {
	/**
	 * Constructs an `Eithered` from a set of functions corresponding to each of Eithered's methods
	 */
	def apply[A, B, Z](leftFn:A => Z, rightFn:B => Z):Eithered[A, B, Z] = {
		final class Apply extends Eithered[A, B, Z] {
			def left(elem:A):Z = leftFn(elem)
			def right(elem:B):Z = rightFn(elem)
		}
		new Apply()
	}

	@ifdef("scalaEpochVersion:2")
	implicit def unitUnit:Eithered[Unit, Unit, Unit] = symmetric[Unit]
	@ifdef("scalaBinaryVersion:3")
	implicit def unitUnit:Eithered[Unit, Unit, Unit] = Eithered.generic

	@ifdef("scalaEpochVersion:2")
	implicit def unitGeneric[B, Z](implicit ev:Optionally[B, Z]):Eithered[Unit, B, Z] = Eithered(_ => ev.none, ev.some _)
	@ifdef("scalaEpochVersion:2")
	implicit def genericUnit[A, Z](implicit ev:Optionally[A, Z]):Eithered[A, Unit, Z] = Eithered(ev.some _, _ => ev.none)

	@ifdef("scalaBinaryVersion:3")
	implicit def unitAny[B, Z](implicit ev:Optionally[B, Z]):Eithered[Unit, B, Z] = Eithered(_ => ev.none, ev.some _)
	@ifdef("scalaBinaryVersion:3")
	implicit def anyUnit[A, Z](implicit ev:Optionally[A, Z]):Eithered[A, Unit, Z] = Eithered(ev.some _, _ => ev.none)

	/**
	 * @version 0.1.1
	 */
	@ifdef("scalaEpochVersion:2")
	trait Eithereds[Expr[+_]] {
		def splicePiece[A]: Eithered[Expr[A], Expr[Iterable[A]], Repeated.SplicePiece[Expr, A]]
	}
	/**
	 * @version 0.1.1
	 */
	@ifdef("scalaEpochVersion:2")
	def forContext(c:scala.reflect.macros.blackbox.Context):Eithereds[c.Expr] = {
		new Eithereds[c.Expr] {
			def splicePiece[A]: Eithered[c.Expr[A], c.Expr[Iterable[A]], Repeated.SplicePiece[c.Expr, A]] =
				Eithered(new Repeated.SplicePiece.One(_), new Repeated.SplicePiece.Many(_))
		}
	}

	/**
	 * @version 0.1.1
	 */
	@ifdef("scalaBinaryVersion:3")
	def quotedSplicePiece[A]: Eithered[scala.quoted.Expr[A], scala.quoted.Expr[Iterable[A]], Repeated.SplicePiece[scala.quoted.Expr, A]] =
			Eithered(new Repeated.SplicePiece.One(_), new Repeated.SplicePiece.Many(_))

	/**
	 * An Eithered that wraps the value in a `scala.Either`

	 * @example
	 * In the following interpolator, even digits are placed in a Left while odd digits are placed in a Right
	 * ```scala
	 * //{
	 * import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._
	 * import name.rayrobdod.stringContextParserCombinator.typeclass.Eithered
	 *
	 * //}
	 * val evenOdd:Interpolator[Either[Char, Char]] = charIn("02468").orElse(charIn("13579"))(using Eithered.discriminatedUnion)
	 *
	 * evenOdd.interpolate(StringContext("4"), Nil) // Left(4): Either[Char, Char]
	 * evenOdd.interpolate(StringContext("7"), Nil) // Right(7): Either[Char, Char]
	 * ```
	 */
	def discriminatedUnion[A, B]:Eithered[A, B, Either[A, B]] = Eithered(Left.apply _, Right.apply _)
}

private[typeclass] trait LowPrioEithered {
	@ifdef("scalaEpochVersion:2")
	implicit def symmetric[A]:Eithered[A, A, A] = Eithered(Predef.identity _, Predef.identity _)

	/**
	 * The fallback Eithered;
	 * creates a union type of the two component types.
	 *
	 * Since the union of a type with itself is equivalent to that same type,
	 * if this Eithered is used for two parsers of the same type,
	 * then the result is a parser of that type.
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def generic[A, B]:Eithered[A, B, A | B] = Eithered.apply(Predef.identity _, Predef.identity _)
}

/**
 * Predefined implicit implementations of ContraEithered
 * and methods to create new ContraEithereds
 */
object ContraEithered extends LowPrioContraEithered {
	/**
	 * Constructs an `ContraEithered` from a set of functions corresponding to each of ContraEithered's methods
	 */
	def apply[Expr[_], A, B, Z](
		contraLeftFn:PartialExprFunction[Expr, Z, A],
		contraRightFn:PartialExprFunction[Expr, Z, B]
	):ContraEithered[Expr, A, B, Z] = {
		final class Apply extends ContraEithered[Expr, A, B, Z] {
			override def contraLeft:PartialExprFunction[Expr, Z, A] = contraLeftFn
			override def contraRight:PartialExprFunction[Expr, Z, B] = contraRightFn
		}
		new Apply()
	}

	@ifdef("scalaEpochVersion:2")
	trait ContraEithereds[Expr[_]] extends LowPrioContraEithereds[Expr] {
		implicit def unitUnit:ContraEithered[Expr, Unit, Unit, Unit]
	}
	@ifdef("scalaEpochVersion:2")
	private[typeclass]
	trait LowPrioContraEithereds[Expr[_]] {
		implicit def symmetric[A]:ContraEithered[Expr, A, A, A]
	}

	@ifdef("scalaEpochVersion:2")
	def forContext(c:scala.reflect.macros.blackbox.Context):ContraEithereds[c.Expr] = {
		val backing = BiEithered.forContext(c)

		new ContraEithereds[c.Expr] {
			implicit override def unitUnit:ContraEithered[c.Expr, Unit, Unit, Unit] = backing.unitUnit
			implicit override def symmetric[A]:ContraEithered[c.Expr, A, A, A] = backing.symmetric[A]
		}
	}

	@ifdef("scalaBinaryVersion:3")
	implicit def quotedUnitUnit(implicit quotes:scala.quoted.Quotes):ContraEithered[scala.quoted.Expr, Unit, Unit, Unit] = quotedSymmetric[Unit]

	implicit def idUnitUnit:ContraEithered[Id, Unit, Unit, Unit] = idSymmetric[Unit]
}

private[typeclass] trait LowPrioContraEithered {
	@ifdef("scalaBinaryVersion:3")
	implicit def quotedSymmetric[A](implicit quotes:scala.quoted.Quotes):ContraEithered[scala.quoted.Expr, A, A, A] = BiEithered.quotedSymmetric

	implicit def idSymmetric[A]:ContraEithered[Id, A, A, A] = BiEithered.idSymmetric
}

/**
 * Predefined implicit implementations of BiEithered
 * and methods to create new BiEithereds
 */
object BiEithered extends LowPrioBiEithered {
	/**
	 * Constructs an `BiEithered` from a set of functions corresponding to each of BiEithered's methods
	 */
	def apply[Expr[_], A, B, Z](
		leftFn:A => Z,
		rightFn:B => Z,
		contraLeftFn:PartialExprFunction[Expr, Z, A],
		contraRightFn:PartialExprFunction[Expr, Z, B]
	):BiEithered[Expr, A, B, Z] = {
		final class Apply extends BiEithered[Expr, A, B, Z] {
			override def left(elem:A):Z = leftFn(elem)
			override def right(elem:B):Z = rightFn(elem)

			override def contraLeft:PartialExprFunction[Expr, Z, A] = contraLeftFn
			override def contraRight:PartialExprFunction[Expr, Z, B] = contraRightFn
		}
		new Apply()
	}

	@ifdef("scalaEpochVersion:2")
	trait BiEithereds[Expr[_]] extends LowPrioBiEithereds[Expr] {
		implicit def unitUnit:BiEithered[Expr, Unit, Unit, Unit]
	}
	@ifdef("scalaEpochVersion:2")
	private[typeclass]
	trait LowPrioBiEithereds[Expr[_]] {
		implicit def symmetric[A]:BiEithered[Expr, A, A, A]
	}
	@ifdef("scalaEpochVersion:2")
	def forContext(c:scala.reflect.macros.blackbox.Context):BiEithereds[c.Expr] = {
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

	@ifdef("scalaBinaryVersion:3")
	implicit def quotedUnitUnit(implicit quotes:scala.quoted.Quotes):BiEithered[scala.quoted.Expr, Unit, Unit, Unit] = quotedSymmetric[Unit]

	@ifdef("scalaBinaryVersion:3")
	implicit def eitherUnitAny[Expr[_], B, Z](implicit ev:BiOptionally[Expr, B, Z]):BiEithered[Expr, Unit, B, Z] = BiEithered(
		_ => ev.none,
		ev.some _,
		PartialExprFunction[Expr, Z, Unit](
			ev.contraNone,
			_ => ()
		),
		ev.contraSome,
	)
	@ifdef("scalaBinaryVersion:3")
	implicit def eitherAnyUnit[Expr[_], A, Z](implicit ev:BiOptionally[Expr, A, Z]):BiEithered[Expr, A, Unit, Z] = BiEithered(
		ev.some _,
		_ => ev.none,
		ev.contraSome,
		PartialExprFunction[Expr, Z, Unit](
			ev.contraNone,
			_ => ()
		),
	)

	@ifdef("scalaBinaryVersion:3")
	implicit def idUnitUnit:BiEithered[Id, Unit, Unit, Unit] = idSymmetric[Unit]
}

private[typeclass] trait LowPrioBiEithered {
	@ifdef("scalaBinaryVersion:3")
	implicit def quotedSymmetric[A](implicit quotes:scala.quoted.Quotes):BiEithered[scala.quoted.Expr, A, A, A] = BiEithered.apply(
		Predef.identity _,
		Predef.identity _,
		PartialExprFunction.identity(scala.quoted.Expr(true)),
		PartialExprFunction.identity(scala.quoted.Expr(true)),
	)

	implicit def idSymmetric[A]:BiEithered[Id, A, A, A] = {
		BiEithered.apply[Id, A, A, A](
			Predef.identity _,
			Predef.identity _,
			PartialExprFunction.identity[Id, A](true),
			PartialExprFunction.identity[Id, A](true),
		)
	}
}
