package com.wavesplatform.lang

import cats.data.EitherT
import cats.kernel.Monoid
import com.wavesplatform.lang.Common.multiplierFunction
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.{CaseType, LazyVal, NativeFunction}

package object compiler {

  val pointType   = CaseType("Point", List("x" -> LONG, "y" -> LONG))
  val listOfLongs = LIST
  val idT         = NativeFunction("idT", 1, 10000: Short, TYPEPARAM('T'), "p1" -> TYPEPARAM('T'))(Right(_))
  val returnsListLong =
    NativeFunction("undefinedOptionLong", 1, 1002: Short, LIST(LONG): TYPE)(_ => ???)
  val idOptionLong =
    NativeFunction("idOptionLong", 1, 1003: Short, UNIT, ("opt" -> UNION(LONG, UNIT)))(_ => Right(()))
  val functionWithTwoPrarmsOfTheSameType =
    NativeFunction("functionWithTwoPrarmsOfTheSameType", 1, 1005: Short, TYPEPARAM('T'), ("p1" -> TYPEPARAM('T')), ("p2" -> TYPEPARAM('T')))(l =>
      Right(l.head))

  val compilerContext = Monoid
    .combine(
      PureContext.ctx,
      CTX(
        Seq(pointType, Common.pointTypeA, Common.pointTypeB),
        Map(
          ("p", (Common.AorB, null)),
          ("l", (LIST(LONG), LazyVal(EitherT.pure(List(1L, 2L))))),
          ("lpa", (LIST(Common.pointTypeA.typeRef), LazyVal(EitherT.pure(List(null, null))))),
          ("lpabc", (LIST(Common.AorBorC), LazyVal(EitherT.pure(List(null, null)))))
        ),
        Seq(multiplierFunction, functionWithTwoPrarmsOfTheSameType, idT, returnsListLong, idOptionLong)
      )
    )
    .compilerContext

}
