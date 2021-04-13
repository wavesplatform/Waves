package com.wavesplatform.lang.v1.evaluator.ctx

import cats.Monad
import cats.implicits._
import com.wavesplatform.lang.v1.compiler.Terms.CaseObj
import com.wavesplatform.lang.v1.compiler.Types.UNIT

package object impl {
  def notImplemented[F[_] : Monad, R](funcName: String, args: List[Any]): F[Either[String, R]] =
      s"Can't apply (${args.map(_.getClass.getSimpleName).mkString(", ")}) to '$funcName'"
        .asLeft[R].pure[F]

  lazy val unit: CaseObj = CaseObj(UNIT, Map.empty)

  def callableResultError(expected: AnyRef, actual: AnyRef): String =
    s"CallableFunction needs to return $expected, but got '$actual'"
}
