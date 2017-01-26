package scorex.transaction.assets

import com.google.common.base.Charsets
import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.TypedTransaction.TransactionType
import scorex.transaction.ValidationError
import scorex.transaction._

import scala.util.{Failure, Success, Try}

sealed trait IssueTransaction extends AssetIssuance {
  def name: Array[Byte]
  def description: Array[Byte]
  def decimals: Byte
  def fee: Long
}

object IssueTransaction extends Deser[IssueTransaction] {

  private case class IssueTransactionImpl(sender: PublicKeyAccount,
                                  name: Array[Byte],
                                  description: Array[Byte],
                                  quantity: Long,
                                  decimals: Byte,
                                  reissuable: Boolean,
                                  fee: Long,
                                  timestamp: Long,
                                  signature: Array[Byte])
      extends IssueTransaction {

    override val assetFee: (Option[AssetId], Long)      = (None, fee)
    override val transactionType: TransactionType.Value = TransactionType.IssueTransaction

    override lazy val assetId = id

    lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte),
                                                sender.publicKey,
                                                BytesSerializable.arrayWithSize(name),
                                                BytesSerializable.arrayWithSize(description),
                                                Longs.toByteArray(quantity),
                                                Array(decimals),
                                                if (reissuable) Array(1: Byte) else Array(0: Byte),
                                                Longs.toByteArray(fee),
                                                Longs.toByteArray(timestamp))

    override lazy val json: JsObject = jsonBase() ++ Json.obj(
        "assetId"     -> Base58.encode(assetId),
        "name"        -> new String(name, Charsets.UTF_8),
        "description" -> new String(description, Charsets.UTF_8),
        "quantity"    -> quantity,
        "decimals"    -> decimals,
        "reissuable"  -> reissuable
      )

    override lazy val balanceChanges: Seq[BalanceChange] =
      Seq(BalanceChange(AssetAcc(sender, Some(assetId)), quantity), BalanceChange(AssetAcc(sender, assetFee._1), -assetFee._2))

    override lazy val bytes: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte), signature, toSign)

  }

  val MaxDescriptionLength = 1000
  val MaxAssetNameLength   = 16
  val MinAssetNameLength   = 4
  val MinFee               = 100000000
  val MaxDecimals          = 8

  override def parseBytes(bytes: Array[Byte]): Try[IssueTransaction] = Try {
    require(bytes.head == TransactionType.IssueTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[IssueTransaction] = Try {
    import EllipticCurveImpl._
    val signature = bytes.slice(0, SignatureLength)
    val txId      = bytes(SignatureLength)
    require(txId == TransactionType.IssueTransaction.id.toByte, s"Signed tx id is not match")
    val sender                        = new PublicKeyAccount(bytes.slice(SignatureLength + 1, SignatureLength + KeyLength + 1))
    val (assetName, descriptionStart) = parseArraySize(bytes, SignatureLength + KeyLength + 1)
    val (description, quantityStart)  = parseArraySize(bytes, descriptionStart)
    val quantity                      = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val decimals                      = bytes.slice(quantityStart + 8, quantityStart + 9).head
    val reissuable                    = bytes.slice(quantityStart + 9, quantityStart + 10).head == (1: Byte)
    val fee                           = Longs.fromByteArray(bytes.slice(quantityStart + 10, quantityStart + 18))
    val timestamp                     = Longs.fromByteArray(bytes.slice(quantityStart + 18, quantityStart + 26))
    IssueTransaction.create(sender, assetName, description, quantity, decimals, reissuable, fee, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  private def createUnverified(sender: PublicKeyAccount,
                               name: Array[Byte],
                               description: Array[Byte],
                               quantity: Long,
                               decimals: Byte,
                               reissuable: Boolean,
                               fee: Long,
                               timestamp: Long,
                               signature: Option[Array[Byte]] = None) =
    if (quantity <= 0) {
      Left(ValidationError.NegativeAmount)
    } else if (description.length > MaxDescriptionLength) {
      Left(ValidationError.TooBigArray)
    } else if (name.length < MinAssetNameLength || name.length > MaxAssetNameLength) {
      Left(ValidationError.InvalidName)
    } else if (decimals < 0 || decimals > MaxDecimals) {
      Left(ValidationError.TooBigArray)
    } else if (!Account.isValid(sender)) {
      Left(ValidationError.InvalidAddress)
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(IssueTransactionImpl(sender, name, description, quantity, decimals, reissuable, fee, timestamp, signature.orNull))
    }

  def create(sender: PublicKeyAccount,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signature: Array[Byte]): Either[ValidationError, IssueTransaction] =
    createUnverified(sender, name, description, quantity, decimals, reissuable, fee, timestamp, Some(signature))
      .right.flatMap(SignedTransaction.verify)

  def create(sender: PrivateKeyAccount,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long): Either[ValidationError, IssueTransaction] =
    createUnverified(sender, name, description, quantity, decimals, reissuable, fee, timestamp).right.map { unverified =>
      unverified.copy(signature = EllipticCurveImpl.sign(sender, unverified.toSign))
    }
}
