package com.wavesplatform.lang

import cats.kernel.Monoid
import com.wavesplatform.lang.Common.multiplierFunction
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.{CaseType, PredefFunction}

package object compiler {

  val pointType = CaseType("Point", List("x" -> LONG, "y" -> LONG))

  val idT = PredefFunction("idT", 1, TYPEPARAM('T'), List("p1" -> TYPEPARAM('T')), 256)(Right(_))
  val extract = PredefFunction("extract", 1, TYPEPARAM('T'), List("p1" -> OPTIONTYPEPARAM(TYPEPARAM('T'))), EXTRACT) {
    case Some(vl) :: Nil => Right(vl)
    case _               => Left("extracting from empty option")
  }
  val undefinedOptionLong = PredefFunction("undefinedOptionLong", 1, OPTION(LONG), List.empty, 257)(_ => ???)
  val idOptionLong        = PredefFunction("idOptionLong", 1, UNIT, List("opt" -> OPTION(OPTION(LONG))), 258)(_ => Right(()))
  val unitOnNone          = PredefFunction("unitOnNone", 1, UNIT, List("opt" -> OPTION(NOTHING)), 259)(_ => Right(()))
  val functionWithTwoPrarmsOfTheSameType =
    PredefFunction("functionWithTwoPrarmsOfTheSameType", 1, TYPEPARAM('T'), List("p1" -> TYPEPARAM('T'), "p2" -> TYPEPARAM('T')), 260)(Right(_))

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
