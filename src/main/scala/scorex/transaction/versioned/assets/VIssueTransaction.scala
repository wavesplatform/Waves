package scorex.transaction.versioned.assets

import com.google.common.base.Charsets
import com.google.common.primitives.{Bytes, Longs}
import monix.eval.Coeval
import play.api.libs.json._
import scorex.account.PublicKeyAccount
import scorex.crypto.signatures.Curve25519.KeyLength
import scorex.serialization.Deser
import scorex.transaction._

import scala.util.{Failure, Success, Try}

final case class VIssueTransaction private (version: Byte,
                                            sender: PublicKeyAccount,
                                            name: Array[Byte],
                                            description: Array[Byte],
                                            quantity: Long,
                                            decimals: Byte,
                                            reissuable: Boolean,
                                            fee: Long,
                                            timestamp: Long,
                                            proofs: Proofs)
    extends ProvenTransaction
    with FastHashId {

  val assetId = id

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      sender.publicKey,
      Longs.toByteArray(timestamp),
      Deser.serializeArray(name),
      Deser.serializeArray(description),
      Longs.toByteArray(quantity),
      Array(decimals),
      Deser.serializeBoolean(reissuable),
      Longs.toByteArray(fee)
    )
  }

  override def builder: TransactionParser = VIssueTransaction

  override def assetFee: (Option[AssetId], Long) = (None, fee)

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      Array(0: Byte),
      bodyBytes(),
      proofs.bytes()
    )
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "version" -> version,
      "assetId"     -> assetId().base58,
      "name"        -> new String(name, Charsets.UTF_8),
      "description" -> new String(description, Charsets.UTF_8),
      "quantity"    -> quantity,
      "decimals"    -> decimals,
      "reissuable"  -> reissuable
    )
  }
}

object VIssueTransaction extends TransactionParserFor[VIssueTransaction] with TransactionParser.MultipleVersions {
  override val typeId: Byte = 13

  override val supportedVersions: Set[Byte] = Set(2)

  val MaxDescriptionLength = 1000
  val MaxAssetNameLength   = 16
  val MinAssetNameLength   = 4
  val MaxDecimals          = 8

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[VIssueTransaction] = {
    for {
      pkts <- parseSenderAndTimestamp(bytes.take(KeyLength + 8))
      (sender, timestamp)           = pkts
      (assetName, descriptionStart) = Deser.parseArraySize(bytes, KeyLength + 8)
      (description, quantityStart)  = Deser.parseArraySize(bytes, descriptionStart)
      quantity <- parseLong(bytes.slice(quantityStart, quantityStart + 8))
      decimals <- bytes
        .slice(quantityStart + 8, quantityStart + 9)
        .headOption
        .fold[Try[Byte]](Failure(new Exception("Cannot parse decimals")))(d => Success(d))
      reissuable <- bytes
        .slice(quantityStart + 9, quantityStart + 10)
        .headOption
        .map(_ == (1: Byte))
        .fold[Try[Boolean]](Failure(new Exception("Cannot parse reissuable flag")))(r => Success(r))
      fee    <- parseLong(bytes.slice(quantityStart + 10, quantityStart + 18))
      proofs <- parseProofs(bytes.drop(quantityStart + 10))
      tx <- create(version, sender, assetName, description, quantity, decimals, reissuable, fee, timestamp, proofs)
        .fold(ve => Failure(new Exception(ve.toString)), t => Success(t))
    } yield tx
  }

  def create(version: Byte,
             sender: PublicKeyAccount,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, VIssueTransaction] =
    validateIssueParams(version, name, description, quantity, decimals, reissuable, fee: Long).map(_ =>
      VIssueTransaction(version, sender, name, description, quantity, decimals, reissuable, fee, timestamp, proofs))

  private def validateIssueParams(version: Byte,
                                  name: Array[Byte],
                                  description: Array[Byte],
                                  quantity: Long,
                                  decimals: Byte,
                                  reissuable: Boolean,
                                  fee: Long): Either[ValidationError, Unit] =
    for {
      _ <- Either.cond(supportedVersions contains version, (), ValidationError.UnsupportedVersion(version))
      _ <- Either.cond(quantity > 0, (), ValidationError.NegativeAmount(quantity, "assets"))
      _ <- Either.cond(description.length <= MaxDescriptionLength, (), ValidationError.TooBigArray)
      _ <- Either.cond(name.length >= MinAssetNameLength && name.length <= MaxAssetNameLength, (), ValidationError.InvalidName)
      _ <- Either.cond(decimals >= 0 && decimals <= MaxDecimals, (), ValidationError.TooBigArray)
      _ <- Either.cond(fee > 0, (), ValidationError.InsufficientFee)
    } yield ()
}
