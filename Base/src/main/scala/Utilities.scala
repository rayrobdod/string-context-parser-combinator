package com.rayrobdod.stringContextParserCombinator

import scala.Predef.refArrayOps
import scala.reflect.api.Universe
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/**
 * Methods to make writing string context macros easier
 */
object Utilities {
	trait Extractor0[A] {
		def unapply(a:A):Boolean
	}
	trait Extractor[A, Z] {
		def unapply(a:A):Option[Z]
	}

	/** A Extractor that matches a Name whose string value is equal to `expecting` */
	def decodedName(expecting:String):Extractor0[Universe#Name] = new Extractor0[Universe#Name] {
		def unapply(res:Universe#Name):Boolean = expecting == res.decodedName.toString
	}
	private[this] val ScalaName = decodedName("scala")
	private[this] val StringContextName = decodedName("StringContext")
	private[this] val ApplyName = decodedName("apply")

	/** An Extractor that matches a StringContext's form */
	def stringContextApply[U <: Context with Singleton](c:U):Extractor[U#Tree, List[U#Expr[String]]] = new Extractor[U#Tree, List[U#Expr[String]]] {
		def unapply(tree:U#Tree):Option[List[U#Expr[String]]] = tree.duplicate match {
			case c.universe.Apply(
				c.universe.Select(
					c.universe.Select(
						c.universe.Ident(ScalaName()),
						StringContextName()
					),
					ApplyName()
				),
				strings
			) => Option(strings.map(x => c.Expr(x)))
			case _ => None
		}
	}

	/**
	 * Match a Tree of the type used for referencing type names
	 */
	def selectChain[U <: Context with Singleton](c:U, name:String):Extractor0[U#Tree] = new Extractor0[U#Tree] {
		def unapply(tree:U#Tree):Boolean = {
			if (name.contains(".")) {
				val (nameInit, nameLast) = {
					val parts = name.split("\\.")
					(String.join(".", parts.init:_*), parts.last)
				}
				val NameLast = decodedName(nameLast)
				val NameInit = selectChain[U](c, nameInit)
				tree.duplicate match {
					// I want to write `case c.universe.Select(NameInit(), NameLast())`, and I
					// think I should be able to, but the compiler explodes whenever I attempt it
					case c.universe.Select(init, NameLast()) if NameInit.unapply(init.asInstanceOf[U#Tree]) => true
					case _ => false
				}
			} else {
				val MyName = decodedName(name)
				tree.duplicate match {
					case c.universe.Ident(MyName()) => true
					case _ => false
				}
			}
		}
	}

	def objectApply(c:Context)(prefix:c.Tree, methodName:String, params:List[c.Tree]):c.Tree = {
		c.universe.Apply(
			c.universe.Select(prefix, MacroCompat.newTermName(c)(methodName)),
			params
		)
	}

	/**
	 * Creates an Expr that represents the concatenation of the component Exprs
	 */
	def concatenateStrings(c:Context)(strings:Seq[c.Expr[String]]):c.Expr[String] = {
		val summedString:c.Tree = strings match {
			case Seq() => c.universe.Literal(c.universe.Constant(""))
			case Seq(x) => x.tree
			case xs:Seq[_] => {
				val accumulatorName = MacroCompat.newTermName(c)("accumulator$")
				val accumulatorType = c.universe.Select(
					c.universe.Select(
						c.universe.Select(
							c.universe.Ident(MacroCompat.newTermName(c)("scala")),
							MacroCompat.newTermName(c)("collection")
						),
						MacroCompat.newTermName(c)("mutable")
					),
					MacroCompat.newTypeName(c)("StringBuilder")
				)
				val stats = scala.collection.mutable.Buffer[c.universe.Tree](
					c.universe.ValDef(
						c.universe.NoMods,
						accumulatorName,
						accumulatorType,
						c.universe.Apply(
							c.universe.Select(
								c.universe.New(accumulatorType),
								MacroCompat.stdTermNames(c).CONSTRUCTOR
							),
							List()
						)
					)
				)
				xs.map(_.asInstanceOf[c.Expr[String]])
					.foreach({x => stats += objectApply(c)(c.universe.Ident(accumulatorName), "append", List(x.tree))})

				c.universe.Block(
					stats.toList,
					c.universe.Select(
						c.universe.Ident(accumulatorName),
						MacroCompat.newTermName(c)("toString")
					)
				)
			}
		}
		c.Expr(summedString)
	}
}
