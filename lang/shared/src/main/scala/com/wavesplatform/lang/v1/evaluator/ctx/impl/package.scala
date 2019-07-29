package com.wavesplatform.lang.v1.evaluator.ctx

import com.wavesplatform.lang.v1.compiler.Terms.{CaseObj, EVALUATED}
import com.wavesplatform.lang.v1.compiler.Types.UNIT

package object impl {
  def notImplemented(funcName: String, args: List[Any]): Either[String, EVALUATED] = Left(
    s"Can't apply (${args.map(_.getClass.getSimpleName).mkString(", ")}) to '$funcName'"
  )

  lazy val unit: CaseObj   = CaseObj(UNIT, Map.empty)
}
