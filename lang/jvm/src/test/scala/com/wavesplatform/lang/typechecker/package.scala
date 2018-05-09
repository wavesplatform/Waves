package com.wavesplatform.lang

import cats.kernel.Monoid
import com.wavesplatform.lang.Common.multiplierFunction
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.TypeChecker.TypeCheckerContext
import com.wavesplatform.lang.v1.ctx.impl.PureContext
import com.wavesplatform.lang.v1.ctx.{Context, PredefFunction, PredefType}
import com.wavesplatform.lang.v1.ctx.impl.PureContext.none

package object typechecker {

  val pointType = PredefType("Point", List("x" -> LONG, "y" -> LONG))

  val idT = PredefFunction("idT", 1, TYPEPARAM('T'), List("p1" -> TYPEPARAM('T')))(Right(_))
  val extract = PredefFunction("extract", 1, TYPEPARAM('T'), List("p1" -> OPTIONTYPEPARAM(TYPEPARAM('T')))) {
    case Some(vl) :: Nil => Right(vl)
    case _               => Left("extracting from empty option")
  }
  val undefinedOptionLong = PredefFunction("undefinedOptionLong", 1, OPTION(LONG), List.empty)(_ => ???)
  val idOptionLong        = PredefFunction("idOptionLong", 1, UNIT, List("opt" -> OPTION(OPTION(LONG))))(_ => Right(()))
  val unitOnNone          = PredefFunction("unitOnNone", 1, UNIT, List("opt" -> OPTION(NOTHING)))(_ => Right(()))
  val functionWithTwoPrarmsOfTheSameType =
    PredefFunction("functionWithTwoPrarmsOfTheSameType", 1, TYPEPARAM('T'), List("p1" -> TYPEPARAM('T'), "p2" -> TYPEPARAM('T')))(Right(_))

  val ctx = Monoid.combine(
    PureContext.instance,
    Context.build(
      Seq(pointType),
      Map(("None", none)),
      functions = Seq(multiplierFunction, functionWithTwoPrarmsOfTheSameType, idT, unitOnNone, undefinedOptionLong, idOptionLong)
    )
  )

  val typeCheckerContext = TypeCheckerContext.fromContext(ctx)
}
