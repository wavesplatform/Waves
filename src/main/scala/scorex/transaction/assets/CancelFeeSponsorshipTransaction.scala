package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.signatures.Curve25519.{KeyLength, SignatureLength}
import scorex.transaction._

import scala.util.{Failure, Success, Try}

case class CancelFeeSponsorshipTransaction private (version: Byte,
                                                    sender: PublicKeyAccount,
                                                    assetId: ByteStr,
                                                    fee: Long,
                                                    timestamp: Long,
                                                    signature: ByteStr)
    extends SignedTransaction
    with FastHashId {

  override val builder: CancelFeeSponsorshipTransaction.type = CancelFeeSponsorshipTransaction

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      Array(builder.typeId),
      sender.publicKey,
      assetId.arr,
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    ))

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version" -> version,
      "assetId" -> assetId.base58
    ))

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte, builder.typeId, version), signature.arr, bodyBytes()))
}

object CancelFeeSponsorshipTransaction extends TransactionParserFor[CancelFeeSponsorshipTransaction] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 15
  override val supportedVersions: Set[Byte] = Set(1)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val signature = ByteStr(bytes.slice(0, SignatureLength))
      val txId      = bytes(SignatureLength)
      require(txId == typeId, s"Signed tx id is not match")
      val sender   = PublicKeyAccount(bytes.slice(SignatureLength + 1, SignatureLength + KeyLength + 1))
      val assetId  = ByteStr(bytes.slice(SignatureLength + KeyLength + 1, SignatureLength + KeyLength + AssetIdLength + 1))
      val feeStart = SignatureLength + KeyLength + AssetIdLength + 1

      val fee       = Longs.fromByteArray(bytes.slice(feeStart, feeStart + 8))
      val timestamp = Longs.fromByteArray(bytes.slice(feeStart + 8, feeStart + 16))
      CancelFeeSponsorshipTransaction
        .create(version, sender, assetId, fee, timestamp, signature)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             assetId: ByteStr,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, TransactionT] =
    if (!supportedVersions.contains(version)) {
      Left(ValidationError.UnsupportedVersion(version))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(CancelFeeSponsorshipTransaction(version, sender, assetId, fee, timestamp, signature))
    }

  def create(version: Byte, sender: PrivateKeyAccount, assetId: ByteStr, fee: Long, timestamp: Long): Either[ValidationError, TransactionT] =
    create(version, sender, assetId, fee, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(sender, unsigned.bodyBytes())))
    }
}
