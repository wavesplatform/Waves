package scorex.transaction.modern.assets

import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.transaction._
import scorex.transaction.base.CancelFeeSponsorshipTxBase
import scorex.transaction.modern.{ModernTransaction, TxData, TxHeader}

import scala.util.Try

final case class CancelFeeSponsorshipPayload(assetId: AssetId) extends TxData {
  override def bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    assetId.arr
  }

  override def json: Coeval[JsObject] = Coeval.evalOnce {
    Json.obj("assetId" -> assetId.base58)
  }
}

case class CancelFeeSponsorshipTx(header: TxHeader, payload: CancelFeeSponsorshipPayload, proofs: Proofs)
    extends ModernTransaction(CancelFeeSponsorshipTx)
    with CancelFeeSponsorshipTxBase
    with FastHashId {
  override val assetFee: (Option[AssetId], Long) = (None, header.fee)
  override val assetId: AssetId                  = payload.assetId
}

object CancelFeeSponsorshipTx extends TransactionParser.Modern[CancelFeeSponsorshipTx, CancelFeeSponsorshipPayload] {

  override val typeId: Byte                 = 15
  override val supportedVersions: Set[Byte] = Set(1)

  override def create(header: TxHeader, data: CancelFeeSponsorshipPayload, proofs: Proofs): Try[CancelFeeSponsorshipTx] =
    Try(CancelFeeSponsorshipTx(header, data, proofs))

  override def parseTxData(version: Byte, bytes: Array[Byte]): Try[(CancelFeeSponsorshipPayload, Int)] = {
    parseByteStr(bytes.take(AssetIdLength))
      .map(id => (CancelFeeSponsorshipPayload(id), AssetIdLength))
  }
}
