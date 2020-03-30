package com.wavesplatform.lang

import cats.kernel.Monoid
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.Common.multiplierFunction
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.ContextfulVal
import com.wavesplatform.lang.v1.evaluator.ctx.NativeFunction
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{PureContext, _}

package object compiler {

  val pointType   = CASETYPEREF("Point", List("x" -> LONG, "y" -> LONG))
  val listOfLongs = LIST
  val idT = NativeFunction[NoContext]("idT", 1, 10000: Short, TYPEPARAM('T'), ("p1", TYPEPARAM('T'))) {
    case a :: Nil => Right(a)
    case _ => ???
  }
  val returnsListLong =
    NativeFunction[NoContext]("undefinedOptionLong", 1, 1002: Short, LIST(LONG): TYPE) { case _ => ??? }
  val idOptionLong =
    NativeFunction[NoContext]("idOptionLong", 1, 1003: Short, UNIT, ("opt", UNION(LONG, UNIT))) { case _ => Right(unit) }
  val functionWithTwoPrarmsOfTheSameType =
    NativeFunction[NoContext]("functionWithTwoPrarmsOfTheSameType",
                   1,
                   1005: Short,
                   TYPEPARAM('T'),
                   ("p1", TYPEPARAM('T')),
                   ("p2", TYPEPARAM('T'))) { case l => Right(l.head) }

  private val arr = ARR(IndexedSeq[EVALUATED](Common.pointAInstance, Common.pointAInstance)).explicitGet()

  val testContext = Monoid
    .combine(
      PureContext.build(Global, V3),
      CTX[NoContext](
        Seq(pointType, Common.pointTypeA, Common.pointTypeB, Common.pointTypeC),
        Map(
          ("p", (Common.AorB, null)),
          ("tv", (Common.AorBorC, null)),
          ("l", (LIST(LONG), ContextfulVal.pure[NoContext](ARR(IndexedSeq(CONST_LONG(1L), CONST_LONG(2L))).explicitGet()))),
          ("lpa", (LIST(Common.pointTypeA), ContextfulVal.pure[NoContext](arr))),
          ("lpabc", (LIST(Common.AorBorC), ContextfulVal.pure[NoContext](arr)))
        ),
        Array(multiplierFunction, functionWithTwoPrarmsOfTheSameType, idT, returnsListLong, idOptionLong)
      )
    )

  val compilerContext = testContext.compilerContext

}
