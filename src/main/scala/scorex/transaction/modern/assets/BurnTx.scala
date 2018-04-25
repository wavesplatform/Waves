package scorex.transaction.modern.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.transaction._
import scorex.transaction.base.BurnTxBase
import scorex.transaction.modern.{ModernTransaction, TxData, TxHeader}
import scorex.transaction.validation.ValidateModern

import scala.util.{Failure, Success, Try}

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

final case class BurnTx(header: TxHeader, payload: BurnPayload, proofs: Proofs)
  extends ModernTransaction(BurnTx)
    with BurnTxBase {
  override def assetFee: (Option[AssetId], Long) = (None, header.fee)

  override val assetId: AssetId = payload.assetId
  override val amount: Long = payload.amount
}

object BurnTx extends TransactionParser.Modern[BurnTx, BurnPayload] {
  override val typeId: Byte = 6

  override def supportedVersions: Set[Byte] = Set(3)

  override def create(header: TxHeader, data: BurnPayload, proofs: Proofs): Try[BurnTx] =
    Try(BurnTx(header, data, proofs))

  override def parseTxData(version: Byte, bytes: Array[Byte]): Try[(BurnPayload, Int)] = {
    for {
      assetId <- parseByteStr(bytes.take(AssetIdLength))
      amount  <- parseLong(bytes.slice(AssetIdLength, AssetIdLength + 8))
      payload <- ValidateModern
        .burnPL(assetId, amount)
        .fold(
          errs => Failure(new Exception(errs.toString())),
          pl => Success(pl)
        )
    } yield (payload, AssetIdLength + 8)
  }
}
