package com.wavesplatform.transaction

import cats.instances.either._
import cats.instances.lazyList._
import cats.syntax.traverse._
import com.wavesplatform.transaction.TxValidationError.InvalidSignature
import monix.eval.Coeval

trait Signed extends Authorized {
  protected val signatureValid: Coeval[Boolean]
  protected def signatureValid(checkPk: Boolean): Boolean

  protected val signedDescendants: Coeval[Seq[Signed]] =
    Coeval(Nil)

  def signaturesValid(checkPk: Boolean): Either[InvalidSignature, this.type] =
    (this +: signedDescendants())
      .to(LazyList)
      .map(
        entity =>
          if (entity.signatureValid(checkPk)) {
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
