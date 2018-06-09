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

case class FunctionTypeSignature(header: FunctionHeader, args: List[TYPEPLACEHOLDER], result: TYPEPLACEHOLDER)

case class PredefFunction(name: String, cost: Long, signature: FunctionTypeSignature, ev: List[Any] => Either[String, Any]) extends BaseFunction {
  def eval(args: List[Any]): TrampolinedExecResult[Any] = EitherT.fromEither[Coeval](ev(args))
}

object PredefFunction {

  def apply(name: String, cost: Long, internalName: Short, args: List[(String, TYPEPLACEHOLDER)], resultType: TYPEPLACEHOLDER)(
      ev: List[Any] => Either[String, Any]) =
    new PredefFunction(name, cost, FunctionTypeSignature(FunctionHeader.Predef(internalName), args.map(_._2), resultType), ev)

}

case class UserFunction(name: String, cost: Long, signature: FunctionTypeSignature, ev: List[EXPR] => Either[String, EXPR]) extends BaseFunction

object UserFunction {

  def apply(name: String, cost: Long, args: List[(String, TYPEPLACEHOLDER)], resultType: TYPEPLACEHOLDER)(ev: List[EXPR] => Either[String, EXPR]) =
    new UserFunction(name, cost, FunctionTypeSignature(FunctionHeader.User(name), args.map(_._2), resultType), ev)

}
