package com.wavesplatform.lang

import cats.data.EitherT
import cats.kernel.Monoid
import com.wavesplatform.lang.Common.multiplierFunction
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{PureContext, _}
import com.wavesplatform.lang.v1.evaluator.ctx.{LazyVal, NativeFunction}

package object compiler {

  val pointType   = CASETYPEREF("Point", List("x" -> LONG, "y" -> LONG))
  val listOfLongs = LIST
  val idT = NativeFunction("idT", 1, 10000: Short, TYPEPARAM('T'), "test id", ("p1", TYPEPARAM('T'), "p1")) {
    case a :: Nil => Right(a)
  }
  val returnsListLong =
    NativeFunction("undefinedOptionLong", 1, 1002: Short, LIST(LONG): TYPE, "test undefinedOptionLong") { case _ => ??? }
  val idOptionLong =
    NativeFunction("idOptionLong", 1, 1003: Short, UNIT, "test Some", ("opt", UNION(LONG, UNIT), "opt")) { case _ => Right(unit) }
  val functionWithTwoPrarmsOfTheSameType =
    NativeFunction("functionWithTwoPrarmsOfTheSameType",
                   1,
                   1005: Short,
                   TYPEPARAM('T'),
                   "test same type params",
                   ("p1", TYPEPARAM('T'), "p1"),
                   ("p2", TYPEPARAM('T'), "p2")) { case l => Right(l.head) }

  private val arr = ARR(IndexedSeq[EVALUATED](null, null))
  val testContext = Monoid
    .combine(
      PureContext.build(Global, V3),
      CTX(
        Seq(pointType, Common.pointTypeA, Common.pointTypeB, Common.pointTypeC),
        Map(
          ("p", ((Common.AorB, "Test variable"), null)),
          ("tv", ((Common.AorBorC, "Yet test variable"), null)),
          ("l", ((LIST(LONG), "Test list"), LazyVal(EitherT.pure(ARR(IndexedSeq(CONST_LONG(1L), CONST_LONG(2L))))))),
          ("lpa", ((LIST(Common.pointTypeA), "Yet test list"), LazyVal(EitherT.pure(arr)))),
          ("lpabc", ((LIST(Common.AorBorC), "Yet another test list"), LazyVal(EitherT.pure(arr))))
        ),
        Array(multiplierFunction, functionWithTwoPrarmsOfTheSameType, idT, returnsListLong, idOptionLong)
      )
    )

  val compilerContext = testContext.compilerContext

}
