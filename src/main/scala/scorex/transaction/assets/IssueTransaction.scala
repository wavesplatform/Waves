package scorex.transaction.assets

import com.google.common.base.Charsets
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.signatures.Curve25519.{KeyLength, SignatureLength}
import scorex.serialization.Deser
import scorex.transaction._

import scala.util.{Failure, Success, Try}

case class IssueTransaction private (sender: PublicKeyAccount,
                                     name: Array[Byte],
                                     description: Array[Byte],
                                     quantity: Long,
                                     decimals: Byte,
                                     reissuable: Boolean,
                                     fee: Long,
                                     timestamp: Long,
                                     signature: ByteStr)
    extends SignedTransaction
    with FastHashId {

  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override val builder: IssueTransaction.type    = IssueTransaction

  val assetId = id

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      Array(builder.typeId),
      sender.publicKey,
      Deser.serializeArray(name),
      Deser.serializeArray(description),
      Longs.toByteArray(quantity),
      Array(decimals),
      Deser.serializeBoolean(reissuable),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    ))

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "assetId"     -> assetId().base58,
      "name"        -> new String(name, Charsets.UTF_8),
      "description" -> new String(description, Charsets.UTF_8),
      "quantity"    -> quantity,
      "decimals"    -> decimals,
      "reissuable"  -> reissuable
    ))

  override val bytes = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), signature.arr, bodyBytes()))

}

object IssueTransaction extends TransactionParserFor[IssueTransaction] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = 3

  val MaxDescriptionLength = 1000
  val MaxAssetNameLength   = 16
  val MinAssetNameLength   = 4
  val MaxDecimals          = 8

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val signature = ByteStr(bytes.slice(0, SignatureLength))
      val txId      = bytes(SignatureLength)
      require(txId == typeId, s"Signed tx id is not match")
      val sender                        = PublicKeyAccount(bytes.slice(SignatureLength + 1, SignatureLength + KeyLength + 1))
      val (assetName, descriptionStart) = Deser.parseArraySize(bytes, SignatureLength + KeyLength + 1)
      val (description, quantityStart)  = Deser.parseArraySize(bytes, descriptionStart)
      val quantity                      = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
      val decimals                      = bytes.slice(quantityStart + 8, quantityStart + 9).head
      val reissuable                    = bytes.slice(quantityStart + 9, quantityStart + 10).head == (1: Byte)
      val fee                           = Longs.fromByteArray(bytes.slice(quantityStart + 10, quantityStart + 18))
      val timestamp                     = Longs.fromByteArray(bytes.slice(quantityStart + 18, quantityStart + 26))
      IssueTransaction
        .create(sender, assetName, description, quantity, decimals, reissuable, fee, timestamp, signature)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(sender: PublicKeyAccount,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, TransactionT] =
    validateIssueParams(name, description, quantity, decimals, reissuable, fee: Long).map(_ =>
      IssueTransaction(sender, name, description, quantity, decimals, reissuable, fee, timestamp, signature))

  def create(sender: PrivateKeyAccount,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long): Either[ValidationError, TransactionT] =
    create(sender, name, description, quantity, decimals, reissuable, fee, timestamp, ByteStr.empty).right.map { unverified =>
      unverified.copy(signature = ByteStr(crypto.sign(sender, unverified.bodyBytes())))
    }

  def validateIssueParams(name: Array[Byte],
                          description: Array[Byte],
                          quantity: Long,
                          decimals: Byte,
                          reissuable: Boolean,
                          fee: Long): Either[ValidationError, Unit] =
    for {
      _ <- Either.cond(quantity > 0, (), ValidationError.NegativeAmount(quantity, "assets"))
      _ <- Either.cond(description.length <= MaxDescriptionLength, (), ValidationError.TooBigArray)
      _ <- Either.cond(name.length >= MinAssetNameLength && name.length <= MaxAssetNameLength, (), ValidationError.InvalidName)
      _ <- Either.cond(decimals >= 0 && decimals <= MaxDecimals, (), ValidationError.TooBigArray)
      _ <- Either.cond(fee > 0, (), ValidationError.InsufficientFee())
    } yield ()
}
