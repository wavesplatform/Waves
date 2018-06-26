package com.wavesplatform.lang.v1.ctx

import cats.data.EitherT
import com.wavesplatform.lang.v1.Terms.TYPE
import com.wavesplatform.lang.TrampolinedExecResult
import monix.eval.Coeval

sealed trait LazyVal {
  val tpe: TYPE
  val value: TrampolinedExecResult[tpe.Underlying]
}

object LazyVal {
  private case class LazyValImpl(tpe: TYPE, v: TrampolinedExecResult[Any]) extends LazyVal {
    override val value: TrampolinedExecResult[tpe.Underlying] = EitherT(Coeval.evalOnce(v.map(_.asInstanceOf[tpe.Underlying]).value.apply()))
  }

  def apply(t: TYPE)(v: TrampolinedExecResult[t.Underlying]): LazyVal = LazyValImpl(t, v.map(_.asInstanceOf[Any]))
}
