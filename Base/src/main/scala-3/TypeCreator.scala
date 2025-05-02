package name.rayrobdod.stringContextParserCombinator

import scala.quoted.*
import scala.annotation.nowarn

/**
 * A factory for types.
 */
abstract class TypeCreator[A <: AnyKind] {
	def createType(using Quotes): Type[A]

	final def show(using Quotes): String = Type.show[A](using this.createType)
}

object TypeCreator {
	def apply[A <: AnyKind](using tt: TypeCreator[A]): TypeCreator[A] = tt

	@nowarn("id=E197")
	inline given derived[A <: AnyKind]: TypeCreator[A] = {
		final class typeCreator extends TypeCreator[A] {
			def createType(using Quotes) = Type.of[A]
		}
		new typeCreator
	}
}
