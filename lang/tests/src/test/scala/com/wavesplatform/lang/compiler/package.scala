package com.wavesplatform.lang

import cats.kernel.Monoid
import com.wavesplatform.common.utils.*
import com.wavesplatform.lang.Common.multiplierFunction
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.compiler.{CompilerContext, Types}
import com.wavesplatform.lang.v1.evaluator.ContextfulVal
import com.wavesplatform.lang.v1.evaluator.ctx.NativeFunction
import com.wavesplatform.lang.v1.evaluator.ctx.impl.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext

package object compiler {

  val pointType: CASETYPEREF       = CASETYPEREF("Point", List("x" -> LONG, "y" -> LONG))
  val listOfLongs: Types.LIST.type = LIST
  val idT: NativeFunction = NativeFunction("idT", 1, 10000: Short, TYPEPARAM('T'), ("p1", TYPEPARAM('T'))) {
    case a :: Nil => Right(a)
    case _        => ???
  }
  val returnsListLong: NativeFunction =
    NativeFunction("undefinedOptionLong", 1, 1002: Short, LIST(LONG): TYPE)(_ => ???)
  val idOptionLong: NativeFunction =
    NativeFunction("idOptionLong", 1, 1003: Short, UNIT, ("opt", UNION(LONG, UNIT)))(_ => Right(unit))
  val functionWithTwoPrarmsOfTheSameType: NativeFunction =
    NativeFunction("functionWithTwoPrarmsOfTheSameType", 1, 1005: Short, TYPEPARAM('T'), ("p1", TYPEPARAM('T')), ("p2", TYPEPARAM('T')))(l =>
      Right(l.head)
    )

  private val arr = ARR(IndexedSeq[EVALUATED](Common.pointAInstance, Common.pointAInstance), false).explicitGet()

  def getTestContext(v: StdLibVersion, t: ScriptType = Account): CTX = {
    Monoid
      .combineAll(
        Seq(
          PureContext.build(v, useNewPowPrecision = true),
          CryptoContext.build(Global, v),
          WavesContext.build(Global, DirectiveSet(v, t, Expression).explicitGet(), fixBigScriptField = true),
          CTX(
            Seq(pointType, Common.pointTypeA, Common.pointTypeB, Common.pointTypeC),
            Map(
              ("p", (Common.AorB, null)),
              ("tv", (Common.AorBorC, null)),
              ("l", (LIST(LONG), ContextfulVal.pure(ARR(IndexedSeq(CONST_LONG(1L), CONST_LONG(2L)), false).explicitGet()))),
              ("lpa", (LIST(Common.pointTypeA), ContextfulVal.pure(arr))),
              ("lpabc", (LIST(Common.AorBorC), ContextfulVal.pure(arr)))
            ),
            Array(multiplierFunction, functionWithTwoPrarmsOfTheSameType, idT, returnsListLong, idOptionLong)
          )
        )
      )
  }

  val compilerContext: CompilerContext   = getTestContext(V3).compilerContext
  val compilerContextV4: CompilerContext = getTestContext(V4).compilerContext
}
