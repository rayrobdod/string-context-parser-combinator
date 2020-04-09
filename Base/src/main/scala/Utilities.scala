package com.rayrobdod.stringContextParserCombinator

import scala.Predef.refArrayOps
import scala.reflect.api.Universe
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/**
 * Methods to make writing string context macros easier
 */
object Utilities {
	private[stringContextParserCombinator]
	trait Extractor0[A] {
		def unapply(a:A):Boolean
	}
	private[stringContextParserCombinator]
	trait Extractor[A, Z] {
		def unapply(a:A):Option[Z]
	}
	private[stringContextParserCombinator]
	trait ExtractorApplicator0[A] extends Extractor0[A] with Function0[A]

	/** A Extractor that matches a Name whose string value is equal to `expecting` */
	private[stringContextParserCombinator]
	def decodedName(expecting:String):Extractor0[Universe#Name] = new Extractor0[Universe#Name] {
		def unapply(res:Universe#Name):Boolean = expecting == res.decodedName.toString
	}
	private[this] val ScalaName = decodedName("scala")
	private[this] val StringContextName = decodedName("StringContext")
	private[this] val ApplyName = decodedName("apply")

	/** An Extractor that matches a StringContext's form */
	private[stringContextParserCombinator]
	def stringContextApply(c:Context):Extractor[c.Tree, List[c.Expr[String]]] = new Extractor[c.Tree, List[c.Expr[String]]] {
		def unapply(tree:c.Tree):Option[List[c.Expr[String]]] = tree.duplicate match {
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
	private[stringContextParserCombinator]
	def selectChain(c:Context, name:String):ExtractorApplicator0[c.Tree] = new ExtractorApplicator0[c.Tree] {
		def unapply(tree:c.Tree):Boolean = {
			if (name.contains(".")) {
				val (nameInit, nameLast) = {
					val parts = name.split("\\.")
					(String.join(".", parts.init:_*), parts.last)
				}
				val NameLast = decodedName(nameLast)
				val NameInit = selectChain(c, nameInit)
				tree.duplicate match {
					// I want to write `case c.universe.Select(NameInit(), NameLast())`, and I
					// think I should be able to, but the compiler explodes whenever I attempt it
					case c.universe.Select(init, NameLast()) if NameInit.unapply(init) => true
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
		def apply:c.Tree = {
			if (name.contains(".")) {
				val (nameInit, nameLast) = {
					val parts = name.split("\\.")
					(String.join(".", parts.init:_*), parts.last)
				}
				c.universe.Select(
					selectChain(c, nameInit).apply,
					MacroCompat.newTermName(c)(nameLast)
				)
			} else {
				c.universe.Ident(
					MacroCompat.newTermName(c)(name)
				)
			}
		}
	}

	/**
	 * Creates an Expr that represents the concatenation of the component Exprs
	 */
	def concatenateStrings(c:Context)(strings:Seq[c.Expr[String]]):c.Expr[String] = {
		strings match {
			case Seq() => c.universe.reify("")
			case Seq(x) => x
			case _ => {
				val accumulatorName = MacroCompat.newTermName(c)("accumulator$")
				val accumulatorType = c.universe.typeTag[scala.collection.mutable.StringBuilder]
				val accumulatorTypeTree = c.universe.TypeTree(accumulatorType.tpe)
				val accumulatorExpr = c.Expr(c.universe.Ident(accumulatorName))(accumulatorType)
				val stats = scala.collection.mutable.Buffer[c.universe.Tree](
					c.universe.ValDef(
						c.universe.NoMods,
						accumulatorName,
						accumulatorTypeTree,
						c.universe.Apply(
							c.universe.Select(
								c.universe.New(accumulatorTypeTree),
								MacroCompat.stdTermNames(c).CONSTRUCTOR
							),
							List()
						)
					)
				)
				strings.foreach(x => stats += c.universe.reify(accumulatorExpr.splice.append(x.splice)).tree)

				c.Expr[String](
					c.universe.Block(
						stats.toList,
						c.universe.reify(accumulatorExpr.splice.toString).tree
					)
				)
			}
		}
	}
}
