package name.rayrobdod.stringContextParserCombinator

import scala.reflect.macros.blackbox.Context

private[stringContextParserCombinator]
object AssembleUnapply {
	def zero[UnexprA](c:Context)(
		value:c.Expr[UnexprA],
		valueType:c.TypeTag[UnexprA],
		conditionFn:c.Expr[UnexprA] => c.Expr[Boolean],
	):c.Expr[Boolean] = {
		import c.universe.Quasiquote

		val valueName = c.freshName(c.universe.TermName("value"))
		val valueIdent = c.Expr[UnexprA](c.universe.Ident(valueName))

		val condition = conditionFn(valueIdent)

		c.Expr[Boolean](
			q"""
				({($valueName: $valueType) => $condition}
					:_root_.name.rayrobdod.stringContextParserCombinator.Unapply.Zero[$valueType])
					.unapply($value)
			"""
		)
	}

	def one[UnexprA](c:Context)(
		value:c.Expr[UnexprA],
		valueType:c.TypeTag[UnexprA],
		conditionFn:c.Expr[UnexprA] => c.Expr[Boolean],
		extracted:UnapplyExpr.Part[c.Expr, c.TypeTag, c.Expr[UnexprA], _],
	):c.Expr[Any] = {
		import c.universe.Quasiquote

		val valueName = c.freshName(c.universe.TermName("value"))
		val valueIdent = c.Expr[UnexprA](c.universe.Ident(valueName))

		val condition = conditionFn(valueIdent)
		val extractedType = extracted.typ
		val extractedValue = extracted.value(valueIdent)

		c.Expr(
			q"""
				({($valueName: $valueType) => Option.when($condition)($extractedValue)}
					:name.rayrobdod.stringContextParserCombinator.Unapply.Fixed[$valueType, $extractedType])
					.unapply($value)
			"""
		)(extractedType)
	}

	def many[UnexprA](c:Context)(
		value:c.Expr[UnexprA],
		valueType:c.TypeTag[UnexprA],
		conditionFn:c.Expr[UnexprA] => c.Expr[Boolean],
		extracteds:List[UnapplyExpr.Part[c.Expr, c.TypeTag, c.Expr[UnexprA], _]],
	):c.Expr[Any] = {
		import c.universe.Quasiquote

		val valueName = c.freshName(c.universe.TermName("value"))
		val valueIdent = c.Expr[UnexprA](c.universe.Ident(valueName))

		val condition = conditionFn(valueIdent)
		val extractedTypes = extracteds.map(_.typ.tpe)
		val extractedValues = extracteds.map(_.value(valueIdent).tree)

		val extractedTuple = {
			c.universe.Apply(
				c.universe.Select(
					c.universe.Select(
						c.universe.Ident(c.universe.TermName("scala")),
						c.universe.TermName(s"Tuple${extracteds.size}")
					),
					c.universe.TermName("apply")
				),
				extractedValues
			)
		}

		val extractedType = {
			c.universe.appliedType(
				c.universe.definitions.TupleClass(extracteds.size).asType.toTypeConstructor,
				extractedTypes
			)
		}

		c.Expr(
			q"""
				({($valueName: $valueType) => Option.when($condition)($extractedTuple)}
					:name.rayrobdod.stringContextParserCombinator.Unapply.Fixed[$valueType, $extractedType])
					.unapply($value)
			"""
		)(c.WeakTypeTag(extractedType))
	}
}
