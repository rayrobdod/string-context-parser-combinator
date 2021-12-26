package com.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import scala.reflect.macros.blackbox.Context

private[datetime] trait FromExpr[A] {
	def unapply[C <: Context with Singleton](ctx:C)(expr:C#Expr[A]):Option[A]
}

private[datetime] object FromExpr {
	private[this] trait NameType {
		def unapply(input:scala.reflect.api.Universe#Name):Option[String]
	}
	private[this] val Name = new NameType {
		def unapply(input:scala.reflect.api.Universe#Name):Option[String] = Option(input.decodedName.toString)
	}

	private[datetime] trait WithContextType[C <: Context with Singleton] {
		def unapply[A](expr:C#Expr[A])(implicit backing:FromExpr[A]):Option[A]
	}

	private[datetime] def WithContext(ctx:Context):WithContextType[ctx.type] = new WithContextType[ctx.type] {
		def unapply[A](expr:ctx.Expr[A])(implicit backing:FromExpr[A]):Option[A] = {
			backing.unapply[ctx.type](ctx)(expr)
		}
	}

	implicit object given_FromExpr_Int extends FromExpr[Int] {
		def unapply[C <: Context with Singleton](ctx:C)(expr:C#Expr[Int]):Option[Int] = {
			import ctx.universe._
			expr match {
				case ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(x))) => Some(x.asInstanceOf[Int])
				case _ => None
			}
		}
	}

	implicit object given_FromExpr_Year extends FromExpr[Year] {
		def unapply[C <: Context with Singleton](ctx:C)(expr:C#Expr[Year]):Option[Year] = {
			val FromExpr0 = WithContext(ctx)
			import ctx.universe._
			expr match {
				case ctx.Expr(Apply(Select(Select(Select(Ident(Name("java")), Name("time")),
						Name("Year")), Name("of")), List(year))) => {
					ctx.Expr[Int](year) match {
						case FromExpr0(yearRaw) => Some(Year.of(yearRaw))
						case _ => None
					}
				}
				case _ => None
			}
		}
	}

	implicit object given_FromExpr_Month extends FromExpr[Month] {
		def unapply[C <: Context with Singleton](ctx:C)(expr:C#Expr[Month]):Option[Month] = {
			val FromExpr0 = WithContext(ctx)
			import ctx.universe._
			expr match {
				case ctx.Expr(Apply(Select(Select(Select(Ident(Name("java")), Name("time")),
						Name("Month")), Name("of")), List(month))) => {
					ctx.Expr[Int](month) match {
						case FromExpr0(monthRaw) => Some(Month.of(monthRaw))
						case _ => None
					}
				}
				case ctx.Expr(Select(Select(Select(Ident(Name("java")), Name("time")),
						Name("Month")), Name(month))) => {
					Some(Month.valueOf(month))
				}
				case _ => None
			}
		}
	}

	implicit object given_FromExpr_YearMonth extends FromExpr[YearMonth] {
		def unapply[C <: Context with Singleton](ctx:C)(expr:C#Expr[YearMonth]):Option[YearMonth] = {
			val FromExpr0 = WithContext(ctx)
			import ctx.universe._
			expr match {
				case ctx.Expr(Apply(Select(Select(Select(Ident(Name("java")), Name("time")),
						Name("YearMonth")), Name("of")), List(year, month))) => {
					(ctx.Expr[Int](year), ctx.Expr[Int](month)) match {
						case (FromExpr0(yearRaw), FromExpr0(monthRaw)) => Some(YearMonth.of(yearRaw, monthRaw))
						case _ => None
					}
				}
				case _ => None
			}
		}
	}
}
