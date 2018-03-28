package scorex.transaction.smart

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state2._
import monix.eval.Coeval
import play.api.libs.json.Json
import scorex.account._
import scorex.serialization.Deser
import scorex.transaction.TransactionParser.{KeyLength, TransactionType}
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._

import scala.util.{Failure, Success, Try}

case class SetScriptTransaction private(version: Byte,
                                        chainId: Byte,
                                        sender: PublicKeyAccount,
                                        script: Option[Script],
                                        fee: Long,
                                        timestamp: Long,
                                        proofs: Proofs) extends ProvenTransaction with FastHashId {

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(version),
    Array(chainId),
    sender.publicKey,
    Deser.serializeOption(script)(s => Deser.serializeArray(s.bytes().arr)),
    Longs.toByteArray(fee),
    Longs.toByteArray(timestamp)))

  override val transactionType = TransactionType.SetScriptTransaction

  override val assetFee = (None, fee)
  override val json = Coeval.evalOnce(jsonBase() ++ Json.obj(
    "version" -> version,
    "script" -> script.map(_.bytes()))
  )

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(transactionType.id.toByte), bodyBytes(), proofs.bytes()))
}

object SetScriptTransaction {

  private val networkByte = AddressScheme.current.chainId

  def parseTail(bytes: Array[Byte]): Try[SetScriptTransaction] = Try {
    val version = bytes(0)
    val chainId = bytes(1)
    val sender = PublicKeyAccount(bytes.slice(2, KeyLength + 2))
    val (scriptOptEi: Option[Either[ValidationError.ScriptParseError, Script]], scriptEnd) = Deser.parseOption(bytes, KeyLength + 2)(str => Script.fromBytes(Deser.parseArraySize(str, 0)._1))
    val scriptEiOpt = scriptOptEi match {
      case None => Right(None)
      case Some(Right(sc)) => Right(Some(sc))
      case Some(Left(err)) => Left(err)
    }

    val fee = Longs.fromByteArray(bytes.slice(scriptEnd, scriptEnd + 8))
    val timestamp = Longs.fromByteArray(bytes.slice(scriptEnd + 8, scriptEnd + 16))
    (for {
      scriptOpt <- scriptEiOpt
      _ <- Either.cond(version == 1, (), GenericError(s"Unsupported SetScriptTransaction version ${version.toInt}"))
      _ <- Either.cond(chainId == networkByte, (), GenericError(s"Wrong chainId ${chainId.toInt}"))
      proofs <- Proofs.fromBytes(bytes.drop(scriptEnd + 16))
      tx <- create(sender, scriptOpt, fee, timestamp, proofs)
    } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(sender: PublicKeyAccount,
             script: Option[Script],
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, SetScriptTransaction] =
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(new SetScriptTransaction(1, networkByte, sender, script, fee, timestamp, proofs))
    }


  def selfSigned(sender: PrivateKeyAccount,
                 script: Option[Script],
                 fee: Long,
                 timestamp: Long): Either[ValidationError, SetScriptTransaction] = create(sender, script, fee, timestamp, Proofs.empty).right.map { unsigned =>
    unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(sender, unsigned.bodyBytes())))).explicitGet())
  }
}