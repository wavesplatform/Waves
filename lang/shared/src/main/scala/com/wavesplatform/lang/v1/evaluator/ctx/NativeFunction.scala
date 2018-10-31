package com.wavesplatform.lang.v1.evaluator.ctx

import cats.data.EitherT
import com.wavesplatform.lang.TrampolinedExecResult
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}
import com.wavesplatform.lang.v1.compiler.Types._
import monix.eval.Coeval

import scala.annotation.meta.field
import scala.scalajs.js.annotation._

sealed trait BaseFunction {
  @JSExport def signature: FunctionTypeSignature
  @JSExport def header: FunctionHeader = signature.header
  @JSExport def name: String
  @JSExport def docString: String
  @JSExport def argsDoc: Array[(String, String)]
}

object BaseFunction {
  implicit def header(bf: BaseFunction): FunctionHeader = bf.header
}

@JSExportTopLevel("FunctionTypeSignature")
case class FunctionTypeSignature(result: TYPE, args: Seq[(String, TYPE)], header: FunctionHeader)

@JSExportTopLevel("NativeFunction")
case class NativeFunction(@(JSExport @field) name: String,
                          @(JSExport @field) cost: Long,
                          @(JSExport @field) signature: FunctionTypeSignature,
                          ev: List[EVALUATED] => Either[String, EVALUATED],
                          @(JSExport @field) docString: String,
                          @(JSExport @field) argsDoc: Array[(String, String)])
    extends BaseFunction {
  def eval(args: List[EVALUATED]): TrampolinedExecResult[EVALUATED] = EitherT.fromEither[Coeval](ev(args))
}

object NativeFunction {

  def apply(name: String, cost: Long, internalName: Short, resultType: TYPE, docString: String, args: (String, TYPE, String)*)(
      ev: List[EVALUATED] => Either[String, EVALUATED]) =
    new NativeFunction(
      name = name,
      cost = cost,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.Native(internalName)),
      ev = ev,
      docString = docString,
      argsDoc = args.map(a => (a._1 -> a._3)).toArray
    )

}

@JSExportTopLevel("UserFunction")
case class UserFunction(@(JSExport @field) name: String,
                        @(JSExport @field) internalName: String,
                        @(JSExport @field) signature: FunctionTypeSignature,
                        ev: EXPR,
                        @(JSExport @field) docString: String,
                        @(JSExport @field) argsDoc: Array[(String, String)])
    extends BaseFunction

object UserFunction {

  def apply(name: String, resultType: TYPE, docString: String, args: (String, TYPE, String)*)(ev: EXPR): UserFunction =
    UserFunction(name, name, resultType, docString, args: _*)(ev)

  def apply(name: String, internalName: String, resultType: TYPE, docString: String, args: (String, TYPE, String)*)(ev: EXPR): UserFunction =
    new UserFunction(
      name = name,
      internalName = internalName,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.User(internalName)),
      ev = ev,
      docString = docString,
      argsDoc = args.map(a => (a._1 -> a._3)).toArray
    )
}
