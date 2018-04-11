package com.wavesplatform.lang.v1

import com.wavesplatform.lang.ScriptExpr
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.v1.Terms.Typed

final case class ScriptExprV1(expr: Typed.EXPR) extends ScriptExpr {
  override type V = V1.type
  override val version: V = V1
}
