package com.wavesplatform.lang.ctx

import cats.data.EitherT
import com.wavesplatform.lang.Terms.{FUNCTION, TYPEPLACEHOLDER}
import com.wavesplatform.lang.TrampolinedExecResult
import monix.eval.Coeval

sealed trait PredefFunction {
  val name: String
  val args: List[(String, TYPEPLACEHOLDER)]
  val resultType: TYPEPLACEHOLDER
  def eval(args: List[Any]): TrampolinedExecResult[Any]
  val signature: FUNCTION
}
object PredefFunction {

  case class PredefFunctionImpl(name: String, resultType: TYPEPLACEHOLDER, args: List[(String, TYPEPLACEHOLDER)], ev: List[Any] => Either[String, Any])
      extends PredefFunction {
    override def eval(args: List[Any]): TrampolinedExecResult[Any] = {
      EitherT.fromEither[Coeval](ev(args))
    }
    override lazy val signature = FUNCTION(args.map(_._2), resultType)
  }

  def apply(name: String, resultType: TYPEPLACEHOLDER, args: List[(String, TYPEPLACEHOLDER)])(ev: List[Any] => Either[String, Any]): PredefFunction =
    PredefFunctionImpl(name, resultType, args, ev)

}
