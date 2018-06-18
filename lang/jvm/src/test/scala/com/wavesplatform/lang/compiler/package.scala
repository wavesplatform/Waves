package com.wavesplatform.lang

import cats.kernel.Monoid
import com.wavesplatform.lang.Common.multiplierFunction
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.{CaseType, NativeFunction}

package object compiler {

  val pointType = CaseType("Point", List("x" -> LONG, "y" -> LONG))

  val idT = NativeFunction("idT", 1, 10000: Short, TYPEPARAM('T'), "p1" -> TYPEPARAM('T'))(Right(_))
  val extract = NativeFunction.create("extract", 1, fromNonable(TYPEPARAM('T')) _, List("p1" -> (TYPEPARAM('T'): TYPEPLACEHOLDER)), 1001: Short) {
    case Some(vl) :: Nil => Right(vl)
    case _               => Left("extracting from empty option")
  }
  val undefinedOptionLong = NativeFunction("undefinedOptionLong", 1, 1002: Short, UNION(LONG, UNIT): TYPEPLACEHOLDER)(_ => ???)
  val idOptionLong        = NativeFunction("idOptionLong", 1, 1003: Short, UNIT, ("opt" -> UNION(LONG, UNIT)))(_ => Right(()))
  val unitOnNone          = NativeFunction("unitOnNone", 1, 1004: Short, UNIT, ("opt" -> UNION(NOTHING, UNIT)))(_ => Right(()))
  val functionWithTwoPrarmsOfTheSameType =
    NativeFunction("functionWithTwoPrarmsOfTheSameType", 1, 1005: Short, TYPEPARAM('T'), ("p1" -> TYPEPARAM('T')), ("p2" -> TYPEPARAM('T')))(l =>
      Right(l.head))

  val compilerContext = Monoid
    .combine(
      PureContext.ctx,
      CTX(
        Seq(pointType, Common.pointTypeA, Common.pointTypeB),
        Map(("p", (Common.AorB, null))),
        Seq(multiplierFunction, functionWithTwoPrarmsOfTheSameType, idT, unitOnNone, undefinedOptionLong, idOptionLong)
      )
    )
    .compilerContext

}
