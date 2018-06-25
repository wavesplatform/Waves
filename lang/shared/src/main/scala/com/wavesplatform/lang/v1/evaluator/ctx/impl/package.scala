package com.wavesplatform.lang.v1.evaluator.ctx

package object impl {
  def notImplemented(funcName: String, args: List[Any]): Either[String, Nothing] = Left(
    s"Can't apply (${args.map(_.getClass.getSimpleName).mkString(", ")}) to '$funcName'"
  )
}
