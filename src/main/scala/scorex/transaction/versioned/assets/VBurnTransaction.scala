package scorex.transaction.versioned.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.signatures.Curve25519.KeyLength
import scorex.transaction._

import scala.util.{Failure, Success, Try}

final case class VBurnTransaction private (sender: PublicKeyAccount,
                                           version: Byte,
                                           assetId: ByteStr,
                                           amount: Long,
                                           fee: Long,
                                           timestamp: Long,
                                           proofs: Proofs)
    extends ProvenTransaction
    with FastHashId {

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val amountBytes    = Longs.toByteArray(amount)
    val feeBytes       = Longs.toByteArray(fee)
    val timestampBytes = Longs.toByteArray(timestamp)

    Bytes.concat(
      Array(builder.typeId, version),
      sender.publicKey,
      timestampBytes,
      assetId.arr,
      amountBytes,
      feeBytes
    )
  }

  override def builder: TransactionParser = VBurnTransaction

  override def assetFee: (Option[AssetId], Long) = (None, fee)

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "version" -> version,
      "assetId" -> assetId.base58,
      "amount"  -> amount,
      "fee"     -> fee
    )
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      Array(0: Byte),
      bodyBytes(),
      proofs.bytes()
    )
  }
}

object VBurnTransaction extends TransactionParserFor[VBurnTransaction] with TransactionParser.MultipleVersions {
  override val typeId: Byte = 12

  override def supportedVersions: Set[Byte] = Set(2)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[VBurnTransaction] = {
    val (amountStart, amountLength) = (KeyLength + AssetIdLength, 8)
    val (feeStart, feeLength)       = (amountStart + amountLength, 16)

    for {
      pkts <- parseSenderAndTimestamp(bytes.take(KeyLength + 8))
      (sender, timestamp) = pkts
      assetId <- parseByteStr(bytes.slice(KeyLength, KeyLength + AssetIdLength))
      amount  <- parseLong(bytes.slice(amountStart, amountStart + amountLength))
      fee     <- parseLong(bytes.slice(feeStart, feeStart + feeLength))
      proofs  <- parseProofs(bytes.drop(feeStart + feeLength))
      tx <- create(sender, version, assetId, amount, fee, timestamp, proofs)
        .fold(ve => Failure(new Exception(ve.toString)), ps => Success(ps))
    } yield tx
  }

  def create(sender: PublicKeyAccount,
             version: Byte,
             assetId: ByteStr,
             quantity: Long,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, VBurnTransaction] = {
    if (!supportedVersions.contains(version)) {
      Left(ValidationError.UnsupportedVersion(version))
    } else if (quantity < 0) {
      Left(ValidationError.NegativeAmount(quantity, "assets"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(VBurnTransaction(sender, version, assetId, quantity, fee, timestamp, proofs))
    }
  }

  def selfSigned(sender: PrivateKeyAccount,
                 version: Byte,
                 assetId: ByteStr,
                 quantity: Long,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, VBurnTransaction] = {
    for {
      unsigned <- create(sender, version, assetId, quantity, fee, timestamp, Proofs.empty)
      signature = crypto.sign(sender, unsigned.bodyBytes())
      selfProof <- Proofs.create(Seq(ByteStr(signature)))
    } yield unsigned.copy(proofs = selfProof)
  }
}
