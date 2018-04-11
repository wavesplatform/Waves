package com.wavesplatform.lang

trait ScriptExpr extends Versioned {
  val expr: version.ExprT
}

object ScriptExpr {
  type WithVersion[V0 <: ScriptVersion] = ScriptExpr { type V = V0 }
}
