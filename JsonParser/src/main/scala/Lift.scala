package com.rayrobdod.stringContextParserCombinatorExample.json

import scalajson.ast._

trait Lift[-A, +Z]{
	def apply(value:A):Z
}

object Lift {
	def apply[A, Z](fn:Function1[A, Z]):Lift[A, Z] = new Lift[A, Z] {def apply(value:A) = fn(value)}

	implicit def jvalue[A]:Lift[A, A] =
		Lift(scala.Predef.identity)

	type Boolean[A] = Lift[A, JBoolean]
	implicit val bool:Lift[scala.Boolean, JBoolean] =
		Lift( scalajson.ast.JBoolean.apply _ )

	type Number[A] = Lift[A, JNumber]
	implicit val int:Lift[scala.Int, JNumber] =
		Lift( scalajson.ast.JNumber.apply _ )
	implicit val long:Lift[scala.Long, JNumber] =
		Lift( scalajson.ast.JNumber.apply _ )
	implicit val bigint:Lift[scala.math.BigInt, JNumber] =
		Lift( scalajson.ast.JNumber.apply _ )
	implicit val bigdecimal:Lift[scala.math.BigDecimal, JNumber] =
		Lift( scalajson.ast.JNumber.apply _ )

	type String[A] = Lift[A, JString]
	implicit val string:Lift[java.lang.String, JString] =
		Lift( scalajson.ast.JString.apply _ )

	type Array[A] = Lift[A, JArray]
	implicit def seq[A](implicit child:Lift[A, JValue]) =
		Lift[scala.collection.immutable.Seq[A], JArray](
			xs => JArray(xs.map(child.apply _).toVector)
		)

	type Object[A] = Lift[A, JObject]
	implicit def map[A](implicit child:Lift[A, JValue]) =
		Lift[scala.collection.immutable.Map[java.lang.String, A], JObject](
			xs => JObject(xs.mapValues(child.apply _).toMap)
		)
}
