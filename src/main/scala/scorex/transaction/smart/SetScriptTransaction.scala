package scorex.transaction.smart

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2._
import monix.eval.Coeval
import play.api.libs.json.Json
import scorex.account._
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.TransactionParser.{KeyLength, TransactionType}
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._

import scala.util.{Failure, Success, Try}

case class SetScriptTransaction private(version: Byte,
                                        sender: PublicKeyAccount,
                                        script: Script,
                                        fee: Long,
                                        timestamp: Long,
                                        proof: ByteStr) extends Transaction with ProvenTransaction {

  val toSign: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(version),
    sender.publicKey,
    BytesSerializable.arrayWithSize(script.bytes().arr),
    Longs.toByteArray(fee),
    Longs.toByteArray(timestamp)))

  override val id = toSign.map(bytes => ByteStr(FastCryptographicHash(bytes)))
  override val transactionType = TransactionType.SetScriptTransaction

  override val assetFee = (None, fee)
  override val json = Coeval.evalOnce(Json.obj("type" -> transactionType.id,
    "id" -> id().base58,
    "sender" -> sender.address,
    "senderPublicKey" -> Base58.encode(sender.publicKey),
    "fee" -> assetFee._2,
    "timestamp" -> timestamp,
    "script" -> script.text,
    "proof" -> proof.base58)
  )

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(transactionType.id.toByte), toSign(), BytesSerializable.arrayWithSize(proof.arr)))
}

object SetScriptTransaction {

  def parseTail(bytes: Array[Byte]): Try[SetScriptTransaction] = Try {
    val version = bytes(0)
    val sender = PublicKeyAccount(bytes.slice(1, KeyLength + 1))
    val (scriptBytes, scriptEnd) = Deser.parseArraySize(bytes, KeyLength + 1)
    val fee = Longs.fromByteArray(bytes.slice(scriptEnd, scriptEnd + 8))
    val timestamp = Longs.fromByteArray(bytes.slice(scriptEnd + 8, scriptEnd + 16))
    (for {
      _ <- Either.cond(version == 1, (), GenericError(s"Unsupported SetScriptTransaction version ${version.toInt}"))
      script <- Script.fromBytes(scriptBytes)
      (proofBytes, _) = Deser.parseArraySize(bytes, scriptEnd + 16)
      tx <- create(sender, script, fee, timestamp, ByteStr(proofBytes))
    } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten


  def create(sender: PublicKeyAccount,
             script: Script,
             fee: Long,
             timestamp: Long,
             proof: ByteStr): Either[ValidationError, SetScriptTransaction] =
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(new SetScriptTransaction(1, sender, script, fee, timestamp, proof))
    }
}