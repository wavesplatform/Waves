package scorex.transaction.versioned

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state2._
import monix.eval.Coeval
import play.api.libs.json._
import scorex.account.{Alias, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.signatures.Curve25519.KeyLength
import scorex.serialization.Deser
import scorex.transaction._

import scala.util.{Failure, Success, Try}

final case class VCreateAliasTransaction private (sender: PublicKeyAccount, version: Byte, alias: Alias, fee: Long, timestamp: Long, proofs: Proofs)
    extends ProvenTransaction
    with FastHashId {

  override def builder: TransactionParser = VCreateAliasTransaction

  override def assetFee: (Option[AssetId], Long) = (None, fee)

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val aliasBytes     = alias.bytes.arr
    val feeBytes       = Longs.toByteArray(fee)
    val timestampBytes = Longs.toByteArray(timestamp)

    Bytes.concat(
      Array(builder.typeId, version),
      aliasBytes,
      feeBytes,
      timestampBytes
    )
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++
      Json.obj(
        "alias" -> alias.name
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

object VCreateAliasTransaction extends TransactionParserFor[VCreateAliasTransaction] with TransactionParser.MultipleVersions {
  override val typeId: Byte = 14

  override val supportedVersions: Set[Byte] = Set(2)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[VCreateAliasTransaction] = {
    for {
      pkts <- parseSenderAndTimestamp(bytes.take(KeyLength + 8))
      (sender, timestamp)    = pkts
      (aliasBytes, aliasEnd) = Deser.parseArraySize(bytes, KeyLength)
      alias <- Alias
        .fromBytes(aliasBytes)
        .fold(ve => Failure(new Exception(ve.toString)), Success.apply)
      fee    <- parseLong(bytes.slice(aliasEnd, aliasEnd + 8))
      proofs <- parseProofs(bytes.drop(aliasEnd + 16))
      tx <- create(sender, version, alias, fee, timestamp, proofs)
        .fold(ve => Failure(new Exception(ve.toString)), Success.apply)
    } yield tx
  }

  def create(sender: PublicKeyAccount,
             version: Byte,
             alias: Alias,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, VCreateAliasTransaction] = {
    for {
      _ <- Either.cond(supportedVersions contains version, (), ValidationError.UnsupportedVersion(version))
      _ <- Either.cond(fee > 0, (), ValidationError.InsufficientFee)
    } yield VCreateAliasTransaction(sender, version, alias, fee, timestamp, proofs)
  }

  def selfSigned(sender: PrivateKeyAccount,
                 version: Byte,
                 alias: Alias,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, VCreateAliasTransaction] = {

    for {
      unsigned <- create(sender, version, alias, fee, timestamp, Proofs.empty)
      signature = crypto.sign(sender, unsigned.bodyBytes())
      selfProofs <- Proofs.create(Seq(ByteStr(signature)))
    } yield unsigned.copy(proofs = selfProofs)
  }
}
