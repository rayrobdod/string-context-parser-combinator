package com.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import scala.reflect.macros.blackbox.Context

private[datetime] trait ToExpr[A] {
	def apply[C <: Context with Singleton](ctx:C)(expr:A):C#Expr[A]
}

private[datetime] object ToExpr {
	private[datetime] trait WithContextType[C <: Context with Singleton] {
		def apply[A](expr:A)(implicit backing:ToExpr[A]):C#Expr[A]
	}

	private[datetime] def WithContext(ctx:Context):WithContextType[ctx.type] = new WithContextType[ctx.type] {
		def apply[A](expr:A)(implicit backing:ToExpr[A]):ctx.Expr[A] = {
			backing.apply[ctx.type](ctx)(expr)
		}
	}

	implicit object given_ToExpr_Int extends ToExpr[Int] {
		def apply[C <: Context with Singleton](ctx:C)(value:Int):C#Expr[Int] = {
			ctx.Expr[Int](ctx.universe.Literal(ctx.universe.Constant(value)))
		}
	}

	implicit object given_ToExpr_Year extends ToExpr[Year] {
		def apply[C <: Context with Singleton](ctx:C)(value:Year):C#Expr[Year] = {
			val ToExpr0 = WithContext(ctx)
			ctx.Expr[Year](
				ctx.universe.Apply(
					ctx.universe.Select(
						ctx.universe.Select(
							ctx.universe.Select(
								ctx.universe.Ident(ctx.universe.TermName("java")),
								ctx.universe.TermName("time")
							),
							ctx.universe.TermName("Year")
						),
						ctx.universe.TermName("of")
					),
					List(
						ToExpr0(value.getValue()).tree
					)
				)
			)
		}
	}

	implicit object given_ToExpr_Month extends ToExpr[Month] {
		def apply[C <: Context with Singleton](ctx:C)(value:Month):C#Expr[Month] = {
			ctx.Expr[Month](
				ctx.universe.Select(
					ctx.universe.Select(
						ctx.universe.Select(
							ctx.universe.Ident(ctx.universe.TermName("java")),
							ctx.universe.TermName("time")
						),
						ctx.universe.TermName("Month")
					),
					ctx.universe.TermName(value.name())
				)
			)
		}
	}

	implicit object given_ToExpr_YearMonth extends ToExpr[YearMonth] {
		def apply[C <: Context with Singleton](ctx:C)(value:YearMonth):C#Expr[YearMonth] = {
			val ToExpr0 = WithContext(ctx)
			ctx.Expr[YearMonth](
				ctx.universe.Apply(
					ctx.universe.Select(
						ctx.universe.Select(
							ctx.universe.Select(
								ctx.universe.Ident(ctx.universe.TermName("java")),
								ctx.universe.TermName("time")
							),
							ctx.universe.TermName("YearMonth")
						),
						ctx.universe.TermName("of")
					),
					List(
						ToExpr0(value.getYear()).tree,
						ToExpr0(value.getMonthValue()).tree
					)
				)
			)
		}
	}

	implicit object given_ToExpr_LocalDate extends ToExpr[LocalDate] {
		def apply[C <: Context with Singleton](ctx:C)(value:LocalDate):C#Expr[LocalDate] = {
			val ToExpr0 = WithContext(ctx)
			ctx.Expr[LocalDate](
				ctx.universe.Apply(
					ctx.universe.Select(
						ctx.universe.Select(
							ctx.universe.Select(
								ctx.universe.Ident(ctx.universe.TermName("java")),
								ctx.universe.TermName("time")
							),
							ctx.universe.TermName("LocalDate")
						),
						ctx.universe.TermName("of")
					),
					List(
						ToExpr0(value.getYear()).tree,
						ToExpr0(value.getMonthValue()).tree,
						ToExpr0(value.getDayOfMonth()).tree
					)
				)
			)
		}
	}
}
