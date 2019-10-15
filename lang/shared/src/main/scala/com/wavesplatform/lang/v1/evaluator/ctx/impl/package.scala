package com.wavesplatform.lang.v1.evaluator.ctx

import cats.implicits._
import cats.Monad
import com.wavesplatform.lang.v1.compiler.Terms.{CaseObj, EVALUATED}
import com.wavesplatform.lang.v1.compiler.Types.UNIT

package object impl {
  def notImplemented[F[_] : Monad](funcName: String, args: List[Any]): F[Either[String, EVALUATED]] =
      s"Can't apply (${args.map(_.getClass.getSimpleName).mkString(", ")}) to '$funcName'"
        .asLeft[EVALUATED].pure[F]

  lazy val unit: CaseObj = CaseObj(UNIT, Map.empty)
}
