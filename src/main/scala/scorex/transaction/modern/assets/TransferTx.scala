package scorex.transaction.modern.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.AddressOrAlias
import scorex.crypto.encode.Base58
import scorex.serialization.Deser
import scorex.transaction.modern.{ModernTransaction, TxData, TxHeader}
import scorex.transaction.validation.ValidateModern
import scorex.transaction.{AssetId, AssetIdLength, Proofs, TransactionParser}

import scala.util.{Failure, Success, Try}

final case class TransferPayload(recipient: AddressOrAlias,
                                 assetId: Option[AssetId],
                                 feeAssetId: Option[AssetId],
                                 amount: Long,
                                 attachment: Array[Byte])
    extends TxData {

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val assetIdBytes    = assetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte))
    val feeAssetIdBytes = feeAssetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte))
    val amountBytes     = Longs.toByteArray(amount)

    Bytes.concat(
      recipient.bytes.arr,
      assetIdBytes,
      feeAssetIdBytes,
      amountBytes,
      Deser.serializeArray(attachment)
    )
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    Json.obj(
      "recipient"  -> recipient.stringRepr,
      "assetId"    -> assetId.map(_.base58),
      "amount"     -> amount,
      "attachment" -> Base58.encode(attachment)
    )
  }
}

final case class TransferTx(header: TxHeader, payload: TransferPayload, proofs: Proofs) extends ModernTransaction(TransferTx) {
  override def assetFee: (Option[AssetId], Long) = (None, header.fee)
}

object TransferTx extends TransactionParser.Modern[TransferTx, TransferPayload] {
  override val typeId: Byte = 4

  override val supportedVersions: Set[Byte] = Set(3)

  override def create(header: TxHeader, data: TransferPayload, proofs: Proofs): Try[TransferTx] = {
    Try(TransferTx(header, data, proofs))
  }

  override def parseTxData(version: Byte, bytes: Array[Byte]): Try[(TransferPayload, Int)] = {
    Try {
      (for {
        recRes <- AddressOrAlias.fromBytes(bytes, 0)
        (recipient, recipientEnd) = recRes
        (assetIdOpt, assetIdEnd)       = Deser.parseByteArrayOption(bytes, recipientEnd, AssetIdLength)
        (feeAssetIdOpt, feeAssetIdEnd) = Deser.parseByteArrayOption(bytes, assetIdEnd, AssetIdLength)
        amount                         = Longs.fromByteArray(bytes.slice(feeAssetIdEnd, feeAssetIdEnd + 8))
        (attachment, attachEnd)   = Deser.parseArraySize(bytes, feeAssetIdEnd + 8)
        proofs <- Proofs.fromBytes(bytes.drop(attachEnd))
        pl <- ValidateModern
          .transferPL(recipient, assetIdOpt.map(ByteStr.apply), feeAssetIdOpt.map(ByteStr.apply), amount, attachment)
          .toEither
      } yield (pl, attachEnd)).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
  }
}
