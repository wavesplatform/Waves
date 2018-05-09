package com.wavesplatform.lang

import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext

sealed trait ScriptVersion { self =>
  type ExprT
  type CtxT
  val value: Int
}

object ScriptVersion {

  private val versions: Map[Int, ScriptVersion] = Map(1 -> Versions.V1)

  def fromInt(i: Int): Option[ScriptVersion] = versions.get(i)

  object Versions {
    object V1 extends ScriptVersion { self =>
      override type ExprT = Terms.EXPR
      override type CtxT  = EvaluationContext
      override val value: Int = 1
    }
  }
}
