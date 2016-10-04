package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.serialization.Deser
import scorex.transaction.TypedTransaction.TransactionType
import scorex.transaction._

import scala.util.Try

case class IssueTransaction(sender: PublicKeyAccount,
                            assetIdOpt: Option[Array[Byte]],
                            name: Array[Byte],
                            description: Array[Byte],
                            quantity: Long,
                            decimals: Byte,
                            reissuable: Boolean,
                            fee: Long,
                            timestamp: Long,
                            signature: Array[Byte]) extends SignedTransaction {

  import IssueTransaction._

  override val transactionType: TransactionType.Value = TransactionType.IssueTransaction

  lazy val assetId = assetIdOpt.getOrElse(id)

  lazy val toSign: Array[Byte] = Bytes.concat(sender.publicKey,
    assetIdOpt.map(a => (1: Byte) +: a).getOrElse(Array(0: Byte)), arrayWithSize(name), arrayWithSize(description),
    Longs.toByteArray(quantity), Array(decimals), if (reissuable) Array(1: Byte) else Array(0: Byte),
    Longs.toByteArray(fee), Longs.toByteArray(timestamp))

  override lazy val json: JsObject = Json.obj(
    "type" -> transactionType.id,
    "id" -> Base58.encode(id),
    "sender" -> sender.address,
    "assetId" -> Base58.encode(assetId),
    "name" -> Base58.encode(name),
    "description" -> Base58.encode(description),
    "quantity" -> quantity,
    "decimals" -> decimals,
    "reissuable" -> reissuable,
    "fee" -> fee,
    "timestamp" -> timestamp,
    "signature" -> Base58.encode(signature)
  )

  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override lazy val balanceChanges: Seq[BalanceChange] =
    Seq(BalanceChange(AssetAcc(sender, Some(assetId)), quantity),
      BalanceChange(AssetAcc(sender, assetFee._1), -assetFee._2))

  override lazy val bytes: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte), signature, toSign)

  def validate: ValidationResult.Value =
    if (!Account.isValid(sender)) {
      ValidationResult.InvalidAddress
    } else if (quantity <= 0) {
      ValidationResult.NegativeAmount
    } else if (fee < MinFee) {
      ValidationResult.InsufficientFee
    } else if (description.length > MaxDescriptionLength) {
      ValidationResult.TooBigArray
    } else if (name.length < MinAssetNameLength || name.length > MaxAssetNameLength) {
      ValidationResult.InvalidName
    } else if (decimals < 0 || decimals > MaxDecimals) {
      ValidationResult.TooBigArray
    } else if (!signatureValid) {
      ValidationResult.InvalidSignature
    } else ValidationResult.ValidateOke

}

object IssueTransaction extends Deser[IssueTransaction] {
  val MaxDescriptionLength = 1000
  val MaxAssetNameLength = 16
  val MinAssetNameLength = 4
  val MinFee = 100000000
  val MaxDecimals = 8

  override def parseBytes(bytes: Array[Byte]): Try[IssueTransaction] = Try {
    require(bytes.head == TransactionType.IssueTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[IssueTransaction] = Try {
    import EllipticCurveImpl._
    val signature = bytes.slice(0, SignatureLength)
    val sender = new PublicKeyAccount(bytes.slice(SignatureLength, SignatureLength + KeyLength))
    val (assetIdOpt, nameStart) = parseOption(bytes, SignatureLength + KeyLength, AssetIdLength)
    val (assetName, descriptionStart) = parseArraySize(bytes, nameStart)
    val (description, quantityStart) = parseArraySize(bytes, descriptionStart)
    val quantity = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val decimals = bytes.slice(quantityStart + 8, quantityStart + 9).head
    val reissuable = bytes.slice(quantityStart + 9, quantityStart + 10).head == (1: Byte)
    val fee = Longs.fromByteArray(bytes.slice(quantityStart + 10, quantityStart + 18))
    val timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 18, quantityStart + 26))
    IssueTransaction(sender, assetIdOpt, assetName, description, quantity, decimals, reissuable, fee, timestamp, signature)
  }

  def create(sender: PrivateKeyAccount,
             assetIdOpt: Option[Array[Byte]],
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long): IssueTransaction = {
    val unsigned =
      IssueTransaction(sender, assetIdOpt, name, description, quantity, decimals, reissuable, fee, timestamp, null)
    val sig = EllipticCurveImpl.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
  }
}