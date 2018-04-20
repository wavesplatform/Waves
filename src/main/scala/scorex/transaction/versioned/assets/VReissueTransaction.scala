package scorex.transaction.versioned.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.signatures.Curve25519.KeyLength
import scorex.serialization.Deser
import scorex.transaction._

import scala.util.{Failure, Success, Try}

final case class VReissueTransaction private (sender: PublicKeyAccount,
                                              version: Byte,
                                              assetId: ByteStr,
                                              quantity: Long,
                                              reissuable: Boolean,
                                              fee: Long,
                                              timestamp: Long,
                                              proofs: Proofs)
    extends ProvenTransaction
    with FastHashId {

  override val builder = VReissueTransaction

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      Array(builder.typeId, version),
      sender.publicKey,
      assetId.arr,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(quantity),
      if (reissuable) Array(1: Byte) else Array(0: Byte),
      Longs.toByteArray(fee)
    ))

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "assetId"    -> assetId.base58,
      "quantity"   -> quantity,
      "reissuable" -> reissuable
    ))

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      Array(0: Byte),
      bodyBytes(),
      proofs.bytes()
    )
  }
}

object VReissueTransaction extends TransactionParserFor[VReissueTransaction] with TransactionParser.MultipleVersions {

  override val typeId: Byte = 14

  override def supportedVersions: Set[Byte] = Set(2)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[VReissueTransaction] =
    for {
      pkts <- parseSenderAndTimestamp(bytes.take(KeyLength + 8))
      (sender, timestamp) = pkts
      assetId <- parseByteStr(bytes.slice(KeyLength, KeyLength + AssetIdLength))
      (quantityBytes, quantityEnd) = Deser.parseArraySize(bytes, KeyLength + AssetIdLength)
      quantity <- parseLong(quantityBytes)
      reissuable <- bytes
        .slice(quantityEnd + 1, quantityEnd + 2)
        .headOption
        .map(_ == (1: Byte))
        .fold[Try[Boolean]](Failure(new Exception("Cannot parse reissuable flag")))(r => Success(r))
      fee    <- parseLong(bytes.slice(quantityEnd + 2, quantityEnd + 10))
      proofs <- parseProofs(bytes.drop(quantityEnd + 10))
      tx <- create(sender, version, assetId, quantity, reissuable, fee, timestamp, proofs)
        .fold(ve => Failure(new Exception(ve.toString)), t => Success(t))
    } yield tx

  def create(sender: PublicKeyAccount,
             version: Byte,
             assetId: ByteStr,
             quantity: Long,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, VReissueTransaction] =
    if (!supportedVersions.contains(version)) {
      Left(ValidationError.UnsupportedVersion(version))
    } else if (quantity <= 0) {
      Left(ValidationError.NegativeAmount(quantity, "assets"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(VReissueTransaction(sender, version, assetId, quantity, reissuable, fee, timestamp, proofs))
    }

  def selfSigned(sender: PrivateKeyAccount,
                 version: Byte,
                 assetId: ByteStr,
                 quantity: Long,
                 reissuable: Boolean,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, VReissueTransaction] = {
    for {
      unsigned <- create(sender, version, assetId, quantity, reissuable, fee, timestamp, Proofs.empty)
      signature = crypto.sign(sender, unsigned.bodyBytes())
      selfProofs <- Proofs.create(Seq(ByteStr(signature)))
    } yield unsigned.copy(proofs = selfProofs)
  }
}
