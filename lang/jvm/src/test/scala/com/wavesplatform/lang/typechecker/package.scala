package com.wavesplatform.lang

import cats.kernel.Monoid
import com.wavesplatform.lang.Common.multiplierFunction
import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.TypeChecker.TypeCheckerContext
import com.wavesplatform.lang.ctx.impl.PureContext
import com.wavesplatform.lang.ctx.{Context, PredefFunction, PredefType}
import com.wavesplatform.lang.ctx.impl.PureContext.{none, some}

package object typechecker {

  val pointType = PredefType("Point", List("x" -> LONG, "y" -> LONG))

  val idT = PredefFunction("idT", TYPEPARAM('T'), List("p1" -> TYPEPARAM('T')))(Right(_))
  val extract = PredefFunction("extract", TYPEPARAM('T'), List("p1" -> OPTIONTYPEPARAM(TYPEPARAM('T')))) {
    case Some(vl) :: Nil => Right(vl)
    case _               => Left("extracting from empty option")
  }
  val undefinedOptionLong = PredefFunction("undefinedOptionLong", OPTION(LONG), List.empty)(_ => ???)
  val idOptionLong        = PredefFunction("idOptionLong", UNIT, List("opt" -> OPTION(OPTION(LONG))))(_ => Right(()))
  val unitOnNone          = PredefFunction("unitOnNone", UNIT, List("opt" -> OPTION(NOTHING)))(_ => Right(()))
  val functionWithTwoPrarmsOfTheSameType =
    PredefFunction("functionWithTwoPrarmsOfTheSameType", TYPEPARAM('T'), List("p1" -> TYPEPARAM('T'), "p2" -> TYPEPARAM('T')))(Right(_))

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
