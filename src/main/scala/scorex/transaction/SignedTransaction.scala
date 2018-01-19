package scorex.transaction

import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import scorex.crypto.EllipticCurveImpl

trait SignedTransaction extends ProvenTransaction with Signed {

  protected override def proofFieldName = "signature"

  val signature: ByteStr

  def proof : ByteStr = signature

  val signatureValid: Coeval[Boolean] = Coeval.evalOnce(EllipticCurveImpl.verify(signature.arr, bodyBytes(), sender.publicKey))
}
