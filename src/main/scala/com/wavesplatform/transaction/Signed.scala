package com.wavesplatform.transaction

import com.wavesplatform.transaction.ValidationError.InvalidSignature
import monix.eval.Coeval

trait Signed extends Authorized {
  val signatureValid: Coeval[Boolean]
}

object Signed {
  type E[A] = Either[InvalidSignature, A]

  implicit class SignedExt(s: Signed) {
    def sigValidEi(): E[Unit] = Either.cond(s.signatureValid(), (), InvalidSignature(Some(s)))
  }

}
