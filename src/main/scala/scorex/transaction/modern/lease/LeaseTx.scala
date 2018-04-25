package scorex.transaction.modern.lease

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.AddressOrAlias
import scorex.transaction._
import scorex.transaction.base.LeaseTxBase
import scorex.transaction.modern.{ModernTransaction, TxData, TxHeader}
import scorex.transaction.validation.ValidateModern

import scala.util.{Failure, Success, Try}

final case class LeasePayload(amount: Long, recipient: AddressOrAlias) extends TxData {
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      recipient.bytes.arr,
      Longs.toByteArray(amount)
    )
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    Json.obj(
      "recipient" -> recipient.stringRepr,
      "amount"    -> amount
    )
  }
}

final case class LeaseTx(header: TxHeader, payload: LeasePayload, proofs: Proofs)
  extends ModernTransaction(LeaseTx)
    with LeaseTxBase {
  override val assetFee: (Option[AssetId], Long) = (None, header.fee)
  val leaseId: Coeval[ByteStr]                   = id

  override val amount: Long = payload.amount
  override val recipient: AddressOrAlias = payload.recipient
}

object LeaseTx extends TransactionParser.Modern[LeaseTx, LeasePayload] {
  override val typeId: Byte = 8

  override val supportedVersions: Set[Byte] = Set(3)

  override def create(header: TxHeader, data: LeasePayload, proofs: Proofs): Try[LeaseTx] =
    Try(LeaseTx(header, data, proofs))

  override def parseTxData(version: Byte, bytes: Array[Byte]): Try[(LeasePayload, Int)] =
    for {
      recRes <- AddressOrAlias
        .fromBytes(bytes, 0)
        .fold(ve => Failure(new Exception(ve.toString)), Success.apply)
      (recipient, recipientEnd) = recRes
      amountStart               = recipientEnd
      amount <- parseLong(bytes.slice(amountStart, amountStart + 8))
      payload <- ValidateModern
        .leasePL(amount, recipient)
        .fold(
          errs => Failure(new Exception(errs.toString())),
          pl => Success(pl)
        )
    } yield (payload, amountStart + 8)
}
