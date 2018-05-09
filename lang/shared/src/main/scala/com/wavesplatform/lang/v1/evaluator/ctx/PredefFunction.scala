package com.wavesplatform.lang.v1.evaluator.ctx

import cats.data.EitherT
import com.wavesplatform.lang.v1.compiler.Terms.TYPEPLACEHOLDER
import com.wavesplatform.lang.TrampolinedExecResult
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.evaluator.ctx.PredefFunction.FunctionTypeSignature
import monix.eval.Coeval

sealed trait PredefFunction {
  val name: String
  val args: List[(String, TYPEPLACEHOLDER)]
  val cost: Long
  val resultType: TYPEPLACEHOLDER
  def eval(args: List[Any]): TrampolinedExecResult[Any]
  val signature: FunctionTypeSignature
  val header: FunctionHeader
}

object PredefFunction {

  case class FunctionTypeSignature(args: List[TYPEPLACEHOLDER], result: TYPEPLACEHOLDER)

  case class PredefFunctionImpl(name: String,
                                cost: Long,
                                resultType: TYPEPLACEHOLDER,
                                args: List[(String, TYPEPLACEHOLDER)],
                                ev: List[Any] => Either[String, Any])
      extends PredefFunction {
    override def eval(args: List[Any]): TrampolinedExecResult[Any] = {
      EitherT.fromEither[Coeval](ev(args))
    }
    override lazy val signature              = FunctionTypeSignature(args.map(_._2), resultType)
    override lazy val header: FunctionHeader = FunctionHeader(name, args.map(_._2).map(FunctionHeader.FunctionHeaderType.fromTypePlaceholder))
  }

  def apply(name: String, cost: Long, resultType: TYPEPLACEHOLDER, args: List[(String, TYPEPLACEHOLDER)])(
      ev: List[Any] => Either[String, Any]): PredefFunction =
    PredefFunctionImpl(name, cost, resultType, args, ev)

}
