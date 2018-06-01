package com.wavesplatform.lang

import cats.kernel.Monoid
import com.wavesplatform.lang.Common.multiplierFunction
import com.wavesplatform.lang.v1.compiler.CompilerContext
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, PredefCaseType, PredefFunction}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.none

package object typechecker {

  val pointType = PredefCaseType("Point", List("x" -> LONG, "y" -> LONG))

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
    EvaluationContext.build(
      letDefs = Map(("None", none)),
      functions = Seq(multiplierFunction, functionWithTwoPrarmsOfTheSameType, idT, unitOnNone, undefinedOptionLong, idOptionLong)
    )
  )

  val typeCheckerContext = CompilerContext.fromEvaluationContext(ctx, Map("Point" -> pointType), Map("None" -> OPTION(NOTHING)))
}
