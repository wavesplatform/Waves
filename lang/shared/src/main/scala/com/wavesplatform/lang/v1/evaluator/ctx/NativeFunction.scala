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
  def name: String
  def docString: String
  def argsDoc: Seq[(String, String)]
}

object BaseFunction {
  implicit def header(bf: BaseFunction): FunctionHeader = bf.header
}

case class FunctionTypeSignature(result: TYPE, args: Seq[TYPE], header: FunctionHeader)

case class NativeFunction private (name: String, cost: Long, signature: FunctionTypeSignature, ev: List[Any] => Either[String, Any], docString: String, argsDoc: Seq[(String, String)])
    extends BaseFunction {
  def eval(args: List[Any]): TrampolinedExecResult[Any] = EitherT.fromEither[Coeval](ev(args))
}

object NativeFunction {

  def apply(name: String, cost: Long, internalName: Short, resultType: TYPE, docString: String, args: (String, TYPE, String)*)(ev: List[Any] => Either[String, Any]) =
    new NativeFunction(name, cost, FunctionTypeSignature(resultType, args.map(_._2), FunctionHeader.Native(internalName)), ev, docString, args.map(a => (a._1 -> a._3)))

}

case class UserFunction private (name: String, internalName: String, signature: FunctionTypeSignature, ev: List[EXPR] => EXPR, docString: String, argsDoc: Seq[(String, String)]) extends BaseFunction

object UserFunction {

  def apply(name: String, resultType: TYPE, docString: String, args: (String, TYPE, String)*)(ev: List[EXPR] => EXPR): UserFunction =
    UserFunction(name, name, resultType, docString, args: _*)(ev)

  def apply(name: String, internalName: String, resultType: TYPE, docString: String, args: (String, TYPE, String)*)(ev: List[EXPR] => EXPR): UserFunction =
    new UserFunction(name, internalName, FunctionTypeSignature(resultType, args.map(_._2), FunctionHeader.User(internalName)), ev, docString, args.map(a => (a._1 -> a._3)))
}
