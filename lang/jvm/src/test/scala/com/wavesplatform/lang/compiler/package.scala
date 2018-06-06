package com.wavesplatform.lang

import cats.kernel.Monoid
import cats.syntax.semigroup._
import com.wavesplatform.lang.Common.multiplierFunction
import com.wavesplatform.lang.v1.compiler.CompilerContext
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, CaseType, PredefFunction}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.none

package object compiler {

  val pointType = CaseType("Point", List("x" -> LONG, "y" -> LONG))

  val idT = PredefFunction("idT", 1, TYPEPARAM('T'), List("p1" -> TYPEPARAM('T')), "idT")(Right(_))
  val extract = PredefFunction("extract", 1, TYPEPARAM('T'), List("p1" -> OPTIONTYPEPARAM(TYPEPARAM('T'))), "extract") {
    case Some(vl) :: Nil => Right(vl)
    case _               => Left("extracting from empty option")
  }
  val undefinedOptionLong = PredefFunction("undefinedOptionLong", 1, OPTION(LONG), List.empty, "undefinedOptionLong")(_ => ???)
  val idOptionLong        = PredefFunction("idOptionLong", 1, UNIT, List("opt" -> OPTION(OPTION(LONG))), "idOptionLong")(_ => Right(()))
  val unitOnNone          = PredefFunction("unitOnNone", 1, UNIT, List("opt" -> OPTION(NOTHING)), "unitOnNone")(_ => Right(()))
  val functionWithTwoPrarmsOfTheSameType =
    PredefFunction("functionWithTwoPrarmsOfTheSameType",
                   1,
                   TYPEPARAM('T'),
                   List("p1" -> TYPEPARAM('T'), "p2" -> TYPEPARAM('T')),
                   "functionWithTwoPrarmsOfTheSameType")(Right(_))

  val typeCheckerContext = Monoid.combine(
    PureContext.compilerContext,
    CompilerContext.build(
      predefTypes = Seq(pointType, Common.pointTypeA, Common.pointTypeB),
      varDefs = Map("None" -> OPTION(NOTHING), "p" -> Common.AorB),
      functions = Seq(multiplierFunction, functionWithTwoPrarmsOfTheSameType, idT, unitOnNone, undefinedOptionLong, idOptionLong)
    )
  )

}
