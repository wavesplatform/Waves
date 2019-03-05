package com.wavesplatform.lang.v1.evaluator.ctx

import com.wavesplatform.lang.v1.compiler.Terms.CaseObj
import com.wavesplatform.lang.v1.compiler.Types.UNIT

package object impl {
  def notImplemented(funcName: String, args: List[Any]): Nothing = throw new Exception(
    s"Can't apply (${args.map(_.getClass.getSimpleName).mkString(", ")}) to '$funcName'"
  )

  lazy val unit: CaseObj   = CaseObj(UNIT, Map.empty)
}
