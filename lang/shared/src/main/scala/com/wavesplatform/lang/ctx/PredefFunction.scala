package com.wavesplatform.lang.ctx

import cats.data.EitherT
import com.wavesplatform.lang.Terms.{FUNCTION, TYPE}
import com.wavesplatform.lang.TrampolinedExecResult
import monix.eval.Coeval

sealed trait PredefFunction {
  val name: String
  val args: List[(String, TYPE)]
  val resultType: TYPE
  def eval(args: List[Any]): TrampolinedExecResult[resultType.Underlying]
  val signature: FUNCTION
}
object PredefFunction {

  case class PredefFunctionImpl(name: String, resultType: TYPE, args: List[(String, TYPE)], ev: List[Any] => Either[String, Any])
      extends PredefFunction {
    override def eval(args: List[Any]): TrampolinedExecResult[resultType.Underlying] = {
      EitherT.fromEither[Coeval](ev(args).map(_.asInstanceOf[resultType.Underlying]))
    }
    override lazy val signature = FUNCTION(args.map(_._2), resultType)
  }

  def apply(name: String, resultType: TYPE, args: List[(String, TYPE)])(ev: List[Any] => Either[String, resultType.Underlying]): PredefFunction =
    PredefFunctionImpl(name, resultType, args, ev)

}
