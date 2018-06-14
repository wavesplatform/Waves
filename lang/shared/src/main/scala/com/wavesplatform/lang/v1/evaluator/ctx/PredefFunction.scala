package com.wavesplatform.lang.v1.evaluator.ctx

import cats.data.EitherT
import com.wavesplatform.lang.TrampolinedExecResult
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.Types._
import monix.eval.Coeval

sealed trait BaseFunction {
  def signature: FunctionTypeSignature
  def header: FunctionHeader = signature.header
  def cost: Long
  def name: String
}

case class FunctionTypeSignature(result: TYPEPLACEHOLDER, args: Seq[TYPEPLACEHOLDER], header: FunctionHeader)

case class PredefFunction private (name: String, cost: Long, signature: FunctionTypeSignature, ev: List[Any] => Either[String, Any])
    extends BaseFunction {
  def eval(args: List[Any]): TrampolinedExecResult[Any] = EitherT.fromEither[Coeval](ev(args))
}

object PredefFunction {

  def apply(name: String, cost: Long, internalName: Short, resultType: TYPEPLACEHOLDER, args: (String, TYPEPLACEHOLDER)*)(
      ev: List[Any] => Either[String, Any]) =
    new PredefFunction(name, cost, FunctionTypeSignature(resultType, args.map(_._2), FunctionHeader.Predef(internalName)), ev)

}

case class UserFunction private (name: String, cost: Long, signature: FunctionTypeSignature, ev: List[EXPR] => Either[String, EXPR])
    extends BaseFunction

object UserFunction {

  def apply(name: String, cost: Long, resultType: TYPEPLACEHOLDER, args: (String, TYPEPLACEHOLDER)*)(ev: List[EXPR] => Either[String, EXPR]) =
    new UserFunction(name, cost, FunctionTypeSignature(resultType, args.map(_._2), FunctionHeader.User(name)), ev)

}
