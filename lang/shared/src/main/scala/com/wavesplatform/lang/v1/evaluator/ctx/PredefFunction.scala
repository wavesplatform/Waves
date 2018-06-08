package com.wavesplatform.lang.v1.evaluator.ctx

import cats.data.EitherT
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.TrampolinedExecResult
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.evaluator.ctx.PredefFunction.FunctionTypeSignature
import monix.eval.Coeval

sealed trait BaseFunction

case class PredefFunction(name: String, cost: Long, signature: FunctionTypeSignature, ev: List[Any] => Either[String, Any]) extends BaseFunction {
  def header: FunctionHeader = signature.header

  def eval(args: List[Any]): TrampolinedExecResult[Any] = EitherT.fromEither[Coeval](ev(args))
}

object PredefFunction {

  case class FunctionTypeSignature(header: FunctionHeader, args: List[TYPEPLACEHOLDER], result: TYPEPLACEHOLDER)

  def apply(name: String, cost: Long, internalName: Short, args: List[(String, TYPEPLACEHOLDER)], resultType: TYPEPLACEHOLDER)(
      ev: List[Any] => Either[String, Any]) =
    new PredefFunction(name, cost, FunctionTypeSignature(FunctionHeader.Predef(internalName), args.map(_._2), resultType), ev)

}
