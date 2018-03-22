package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.Json
import scorex.account.{AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import scorex.serialization.Deser
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.ValidationError.{GenericError, UnsupportedVersion}
import scorex.transaction.smart.Script
import scorex.transaction._

import scala.util.Try

case class SmartIssueTransaction private (version: Byte,
                                          chainId: Byte,
                                          sender: PublicKeyAccount,
                                          name: Array[Byte],
                                          description: Array[Byte],
                                          quantity: Long,
                                          decimals: Byte,
                                          reissuable: Boolean,
                                          script: Option[Script],
                                          fee: Long,
                                          timestamp: Long,
                                          proofs: Proofs)
    extends ProvenTransaction
    with FastHashId
    with ChainSpecific {

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      Array(version),
      Array(chainId),
      sender.publicKey,
      Deser.serializeArray(name),
      Deser.serializeArray(description),
      Longs.toByteArray(quantity),
      Array(decimals),
      Deser.serializeBoolean(reissuable),
      Deser.serializeOption(script)(s => Deser.serializeArray(s.bytes().arr)),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    ))

  override val transactionType            = TransactionType.SmartIssueTransaction
  override val assetFee                   = (None, fee)
  override val json                       = Coeval.evalOnce(jsonBase() ++ Json.obj("version" -> version, "script" -> script.map(_.text)))
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(transactionType.id.toByte), bodyBytes(), proofs.bytes()))
}

object SmartIssueTransaction {

  private val networkByte = AddressScheme.current.chainId

  def parseBytes(bytes: Array[Byte]): Try[SmartIssueTransaction] = Try {
    require(bytes.head == TransactionType.SmartIssueTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[SmartIssueTransaction] =
    Try {
      val version                       = bytes(0)
      val chainId                       = bytes(1)
      val sender                        = PublicKeyAccount(bytes.slice(2, TransactionParser.KeyLength + 2))
      val (assetName, descriptionStart) = Deser.parseArraySize(bytes, TransactionParser.KeyLength + 2)
      val (description, quantityStart)  = Deser.parseArraySize(bytes, descriptionStart)
      val quantity                      = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
      val decimals                      = bytes.slice(quantityStart + 8, quantityStart + 9).head
      val reissuable                    = bytes.slice(quantityStart + 9, quantityStart + 10).head == (1: Byte)
      val (scriptOptEi: Option[Either[ValidationError.ScriptParseError, Script]], scriptEnd) =
        Deser.parseOption(bytes, quantityStart + 10)(str => Script.fromBytes(Deser.parseArraySize(str, 0)._1))
      val scriptEiOpt: Either[ValidationError.ScriptParseError, Option[Script]] = scriptOptEi match {
        case None            => Right(None)
        case Some(Right(sc)) => Right(Some(sc))
        case Some(Left(err)) => Left(err)
      }
      val fee       = Longs.fromByteArray(bytes.slice(scriptEnd, scriptEnd + 8))
      val timestamp = Longs.fromByteArray(bytes.slice(scriptEnd + 8, scriptEnd + 16))

      (for {
        proofs <- Proofs.fromBytes(bytes.drop(scriptEnd + 16))
        script <- scriptEiOpt
        tx <- SmartIssueTransaction
          .create(version, chainId, sender, assetName, description, quantity, decimals, reissuable, script, fee, timestamp, proofs)
      } yield tx).left.map(e => new Throwable(e.toString)).toTry

    }.flatten

  def create(version: Byte,
             chainId: Byte,
             sender: PublicKeyAccount,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             script: Option[Script],
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, SmartIssueTransaction] =
    for {
      _ <- Either.cond(version == 1, (), UnsupportedVersion(version))
      _ <- Either.cond(chainId == networkByte, (), GenericError(s"Wrong chainId ${chainId.toInt}"))
      _ <- IssueTransaction.validateIssueParams(name, description, quantity, decimals, reissuable, fee)
    } yield SmartIssueTransaction(version, chainId, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, proofs)

  def selfSigned(version: Byte,
                 chainId: Byte,
                 sender: PrivateKeyAccount,
                 name: Array[Byte],
                 description: Array[Byte],
                 quantity: Long,
                 decimals: Byte,
                 reissuable: Boolean,
                 script: Option[Script],
                 fee: Long,
                 timestamp: Long): Either[ValidationError, SmartIssueTransaction] =
    for {
      unverified <- create(version, chainId, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, Proofs.empty)
      proofs     <- Proofs.create(Seq(ByteStr(crypto.sign(sender, unverified.bodyBytes()))))
    } yield unverified.copy(proofs = proofs)
}
