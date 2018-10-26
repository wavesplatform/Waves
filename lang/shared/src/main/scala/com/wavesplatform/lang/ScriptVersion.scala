package com.wavesplatform.lang

import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext

sealed trait ScriptVersion { self =>
  type ExprT = Terms.EXPR
  type CtxT = EvaluationContext
  val value: Int
}

object ScriptVersion {

  private val versions: Map[Int, ScriptVersion] = Map(1 -> Versions.V1, 2 -> Versions.V2)

  def fromInt(i: Int): Option[ScriptVersion] = versions.get(i)

  object Versions {
    object V1 extends ScriptVersion {
      override val value: Int = 1
    }

    object V2 extends ScriptVersion {
      override val value: Int = 2
    }
  }
}
