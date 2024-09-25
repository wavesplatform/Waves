package com.wavesplatform.transaction

import cats.instances.either.*
import cats.instances.lazyList.*
import cats.syntax.traverse.*
import com.wavesplatform.transaction.TxValidationError.InvalidSignature
import monix.eval.Coeval

trait Signed extends Authorized {
  protected val signatureValid: Coeval[Boolean]

  protected val signedDescendants: Coeval[Seq[Signed]] =
    Coeval(Nil)

  val signaturesValid: Coeval[Either[InvalidSignature, this.type]] = Coeval.evalOnce {
    (this +: signedDescendants())
      .to(LazyList)
      .map(entity =>
        if (entity.signatureValid()) {
          Right(entity)
        } else {
          Left(InvalidSignature(entity, None))
        }
      )
      .sequence
      .left
      .map { is =>
        if (is.entity.eq(this)) is
        else InvalidSignature(this, Some(is))
      }
      .map(_ => this)
  }
}
