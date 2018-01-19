package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.TransactionParser._
import scorex.transaction._

import scala.util.{Failure, Success, Try}

case class ScriptTransferTransaction private(version: Byte,
                                             sender: PublicKeyAccount,
                                             recipient: AddressOrAlias,
                                             assetId: Option[AssetId],
                                             amount: Long,
                                             timestamp: Long,
                                             fee: Long,
                                             attachment: Array[Byte],
                                             proof: ByteStr)
  extends ProvenTransaction with FastHashId {
  override val transactionType: TransactionType.Value = TransactionType.ScriptTransferTransaction

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val timestampBytes = Longs.toByteArray(timestamp)
    val assetIdBytes = assetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte))
    val amountBytes = Longs.toByteArray(amount)
    val feeBytes = Longs.toByteArray(fee)

    Bytes.concat(Bytes.concat(Array(version),
      sender.publicKey,
      assetIdBytes,
      timestampBytes,
      amountBytes,
      feeBytes,
      recipient.bytes.arr,
      BytesSerializable.arrayWithSize(attachment)))
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce(jsonBase() ++ Json.obj(
    "version" -> version,
    "recipient" -> recipient.stringRepr,
    "assetId" -> assetId.map(_.base58),
    "amount" -> amount,
    "attachment" -> Base58.encode(attachment)
  ))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(transactionType.id.toByte), bodyBytes(), BytesSerializable.arrayWithSize(proof.arr)))

}


object ScriptTransferTransaction {

  def parseTail(bytes: Array[Byte]): Try[ScriptTransferTransaction] = Try {
    val version = bytes(0)
    val sender = PublicKeyAccount(bytes.slice(1, KeyLength + 1))
    val (assetIdOpt, s0) = Deser.parseOption(bytes, KeyLength + 1, AssetIdLength)
    val timestamp = Longs.fromByteArray(bytes.slice(s0, s0 + 8))
    val amount = Longs.fromByteArray(bytes.slice(s0 + 8, s0 + 16))
    val feeAmount = Longs.fromByteArray(bytes.slice(s0 + 16, s0 + 24))

    (for {
      recRes <- AddressOrAlias.fromBytes(bytes, s0 + 24)
      (recipient, recipientEnd) = recRes
      (attachment, attachEnd) = Deser.parseArraySize(bytes, recipientEnd)
      (proofBytes, _) = Deser.parseArraySize(bytes, attachEnd)
      tt <- ScriptTransferTransaction.create(version,assetIdOpt.map(ByteStr(_)), sender, recipient, amount, timestamp, feeAmount, attachment, ByteStr(proofBytes))
    } yield tt).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(version: Byte,
             assetId: Option[AssetId],
             sender: PublicKeyAccount,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             feeAmount: Long,
             attachment: Array[Byte],
             proof: ByteStr): Either[ValidationError, ScriptTransferTransaction] = {
    if (attachment.length > TransferTransaction.MaxAttachmentSize) {
      Left(ValidationError.TooBigArray)
    } else if (amount <= 0) {
      Left(ValidationError.NegativeAmount(amount, "waves"))
    } else if (Try(Math.addExact(amount, feeAmount)).isFailure) {
      Left(ValidationError.OverflowError)
    } else if (feeAmount <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(ScriptTransferTransaction(version,  sender, recipient, assetId, amount, timestamp, feeAmount, attachment, proof))
    }
  }

  def selfSigned(version: Byte,
                 assetId: Option[AssetId],
                 sender: PrivateKeyAccount,
                 recipient: AddressOrAlias,
                 amount: Long,
                 timestamp: Long,
                 feeAmount: Long,
                 attachment: Array[Byte]): Either[ValidationError, ScriptTransferTransaction] = {
    create(version,assetId,sender,recipient,amount,timestamp,feeAmount,attachment,ByteStr.empty).right.map { unsigned =>
      unsigned.copy(proof = ByteStr(EllipticCurveImpl.sign(sender, unsigned.bodyBytes())))
    }
  }
}



