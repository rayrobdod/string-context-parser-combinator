package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.annotation.nowarn
import scala.reflect.macros.blackbox.Context

/** Prevent the compiler from producing duplicate typecreator classes by using these explicitly instead */
private[typeclass] object TypeTags {

	def string(c: Context): c.TypeTag[String] = c.universe.typeTag[java.lang.String]

	/*
	 * These methods do use the implicit parameters, as evidenced by it won't compile without the parameters,
	 * but the compiler seems to hide that use from itself
	 */

	def option[A](c: Context)(implicit @nowarn("msg=never used") typA: c.TypeTag[A]): c.TypeTag[Option[A]] = c.universe.typeTag[Option[A]]

	def list[A](c: Context)(implicit @nowarn("msg=never used") typA: c.TypeTag[A]): c.TypeTag[List[A]] = c.universe.typeTag[List[A]]
}
