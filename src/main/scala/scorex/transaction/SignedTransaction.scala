package scorex.transaction

import com.wavesplatform.state2._
import monix.eval.Coeval
import scorex.crypto.EllipticCurveImpl

trait SignedTransaction extends ProvenTransaction with Signed {

  protected override def proofField = "signature" -> this.signature.base58

  val signature: ByteStr

  def proofs: Proofs = Proofs.create(Seq(signature)).explicitGet()

  val signatureValid: Coeval[Boolean] = Coeval.evalOnce(EllipticCurveImpl.verify(signature.arr, bodyBytes(), sender.publicKey))
}
