package com.wavesplatform.lang.v1.evaluator.ctx.impl

import cats.implicits._
import cats.{Eval, Monad}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.compiler.Terms._

package object converters {

  implicit def c(b: ByteStr): EVALUATED                = CONST_BYTESTR(b).explicitGet()
  implicit def c(b: String): EVALUATED                 = CONST_STRING(b).explicitGet()
  implicit def c(b: Long): EVALUATED                   = CONST_LONG(b)
  implicit def c(b: Boolean): EVALUATED                = CONST_BOOLEAN(b)
  implicit def c(is: IndexedSeq[EVALUATED]): EVALUATED = ARR(is).explicitGet()
  implicit def c(is: Seq[EVALUATED]): EVALUATED        = ARR(is.toIndexedSeq).explicitGet()

  implicit def fromOptionBV[T](v: Option[ByteStr]): EVALUATED = v.flatMap(CONST_BYTESTR(_).toOption).getOrElse(unit)
  implicit def fromOptionL[T](v: Option[Long]): EVALUATED     = v.map(CONST_LONG).getOrElse(unit)
  implicit def fromOptionS[T](v: Option[String]): EVALUATED   = v.flatMap(CONST_STRING(_).toOption).getOrElse(unit)
  implicit def fromOptionB[T](v: Option[Boolean]): EVALUATED  = v.map(CONST_BOOLEAN).getOrElse(unit)
  implicit def fromOptionCO[T](v: Option[CaseObj]): EVALUATED = v.getOrElse(unit)

  implicit def pure[F[_]: Monad, A <: EVALUATED](
    v: Either[ExecutionError, A]
  ): F[Either[ExecutionError, EVALUATED]] =
    v.asInstanceOf[Either[ExecutionError, EVALUATED]].pure[F]

  implicit def pureEval[F[_], A <: EVALUATED](
    v: Eval[Either[ExecutionError, A]]
  )(implicit m: Monad[F]): Eval[F[Either[ExecutionError, EVALUATED]]] =
    v.map(ei => pure(ei)(m).asInstanceOf[F[Either[ExecutionError, EVALUATED]]])
}
