package scorex.transaction.modern.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.transaction._
import scorex.transaction.modern.{ModernTransaction, TxData, TxHeader}

import scala.util.Try

final case class BurnPayload(assetId: ByteStr, amount: Long) extends TxData {
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      assetId.arr,
      Longs.toByteArray(amount)
    )
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    Json.obj(
      "assetId" -> assetId.base58,
      "amount"  -> amount
    )
  }
}

final case class BurnTx(header: TxHeader,
                        payload: BurnPayload,
                        proofs: Proofs) extends ModernTransaction(BurnTx) {
  override def assetFee: (Option[AssetId], Long) = (None, header.fee)
}

object BurnTx extends TransactionParser.Modern[BurnTx, BurnPayload] {
  override val typeId: Byte = 12

  override def supportedVersions: Set[Byte] = Set(2)

  override def create(header: TxHeader, data: BurnPayload, proofs: Proofs): Try[BurnTx] =
    Try(BurnTx(header, data, proofs))

  override def parseTxData(version: Byte, bytes: Array[Byte]): Try[(BurnPayload, Int)] = {
    for {
      assetId <- parseByteStr(bytes.take(AssetIdLength))
      amount  <- parseLong(bytes.slice(AssetIdLength, AssetIdLength + 8))
    } yield (BurnPayload(assetId, amount), AssetIdLength + 8)
  }
}
