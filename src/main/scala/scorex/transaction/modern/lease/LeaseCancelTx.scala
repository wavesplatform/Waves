package scorex.transaction.modern.lease

import com.wavesplatform.crypto
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.transaction._
import scorex.transaction.modern.{ModernTransaction, TxData, TxHeader}

import scala.util.Try

final case class LeaseCancelPayload(leaseId: ByteStr) extends TxData {
  override val bytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(leaseId.arr)

  override val json: Coeval[JsObject] =
    Coeval.evalOnce(Json.obj("leaseId" -> leaseId.base58))
}

final case class LeaseCancelTx(header: TxHeader,
                               payload: LeaseCancelPayload,
                               proofs: Proofs) extends ModernTransaction(LeaseCancelTx) {
  override val assetFee: (Option[AssetId], Long) = (None, header.fee)
}

object LeaseCancelTx extends TransactionParser.Modern[LeaseCancelTx, LeaseCancelPayload] {
  override val typeId: Byte = 9

  override val supportedVersions: Set[Byte] = Set(2)

  override def create(header: TxHeader, data: LeaseCancelPayload, proofs: Proofs): Try[LeaseCancelTx] = ???

  override def parseTxData(version: Byte, bytes: Array[Byte]): Try[(LeaseCancelPayload, Int)] = {
    parseByteStr(bytes.take(crypto.DigestSize))
      .map(leaseId => (LeaseCancelPayload(leaseId), crypto.DigestSize))
  }
}
