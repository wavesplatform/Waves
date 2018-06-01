package com.wavesplatform.lang.v1.evaluator.ctx

import cats.data.EitherT
import com.wavesplatform.lang.TrampolinedExecResult
import com.wavesplatform.lang.v1.compiler.Terms.TYPE
import com.wavesplatform.lang.v1.evaluator.CoevalRef
import monix.eval.Coeval
import cats.implicits._

sealed trait LazyVal {
  val evaluated: CoevalRef[Boolean] = CoevalRef.of(false)

  val value: TrampolinedExecResult[Any]

  override def toString: String = {
    val valueStringRepr: String = evaluated.read
      .map(ev => {
        if (ev) {
          value.value
            .attempt()
            .fold(
              err => s"Error evaluating value: $err",
              _.fold(
                err => s"Error evaluating value: $err",
                v => v.toString
              )
            )
        } else "Not evaluated"
      }).value

    //s"Type: ${tpe.typeInfo}, Value: $valueStringRepr"
    s"Value: $valueStringRepr"
  }
}

object LazyVal {
  private case class LazyValImpl(v: TrampolinedExecResult[Any]) extends LazyVal {
    override val value: TrampolinedExecResult[Any] = EitherT(
      Coeval.evalOnce(
        evaluated.write(true).apply()
      ) *>
        Coeval.evalOnce(
          v.value.apply()
        )
    )
  }

  def apply(v: TrampolinedExecResult[Any]): LazyVal = LazyValImpl(v)
}
