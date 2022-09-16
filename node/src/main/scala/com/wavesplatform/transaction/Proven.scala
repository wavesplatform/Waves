package com.wavesplatform.transaction
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import monix.eval.Coeval

trait Proven extends Authorized {
  def proofs: Proofs
  val bodyBytes: Coeval[Array[Byte]]
  val firstProofIsValidSignature: Coeval[Boolean] = Coeval.evalOnce {
    proofs.proofs.size == 1 &&
    crypto.verify(proofs.proofs.head, bodyBytes(), sender)
  }
}

object Proven {
  implicit class ProvenExt(private val p: Proven) extends AnyVal {
    def signature: ByteStr = p.proofs.toSignature
  }
}
