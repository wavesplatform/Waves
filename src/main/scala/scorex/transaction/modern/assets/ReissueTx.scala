package scorex.transaction.modern.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.serialization.Deser
import scorex.transaction._
import scorex.transaction.modern.{ModernTransaction, TxData, TxHeader}

import scala.util.{Failure, Success, Try}

final case class ReissuePayload(assetId: ByteStr, quantity: Long, reissuable: Boolean) extends TxData {
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      assetId.arr,
      Longs.toByteArray(quantity),
      if (reissuable) Array(1: Byte) else Array(0: Byte),
    )
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    Json.obj(
      "assetId"    -> assetId.base58,
      "quantity"   -> quantity,
      "reissuable" -> reissuable
    )
  }
}

final case class ReissueTx(header: TxHeader, payload: ReissuePayload, proofs: Proofs) extends ModernTransaction(ReissueTx) {
  override val assetFee: (Option[AssetId], Long) = (None, header.fee)
}

object ReissueTx extends TransactionParser.Modern[ReissueTx, ReissuePayload] {

  override val typeId: Byte = 5

  override val supportedVersions: Set[Byte] = Set(3)

  override def create(header: TxHeader, data: ReissuePayload, proofs: Proofs): Try[ReissueTx] = {
    Try(ReissueTx(header, data, proofs))
  }

  override def parseTxData(version: Byte, bytes: Array[Byte]): Try[(ReissuePayload, Int)] = {
    for {
      assetId <- parseByteStr(bytes.take(AssetIdLength))
      quantity <- parseLong(bytes.slice(AssetIdLength, AssetIdLength + 8))
      reissuable = bytes.slice(AssetIdLength + 8, AssetIdLength + 9).head == (1: Byte)
    } yield (ReissuePayload(assetId, quantity, reissuable), AssetIdLength + 9)
  }
}
