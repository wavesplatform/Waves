package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.Json
import scorex.account.{AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import scorex.serialization.Deser
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

  override val builder: TransactionParser = SmartIssueTransaction

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      Array(0, builder.typeId, version, chainId),
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

  override val assetFee = (None, fee)
  override val json = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version" -> version,
      "script"  -> script.map(_.text)
    ))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(bodyBytes(), proofs.bytes()))
}

object SmartIssueTransaction extends TransactionParserFor[SmartIssueTransaction] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 3
  override val supportedVersions: Set[Byte] = Set(2)

  private val networkByte = AddressScheme.current.chainId

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val chainId                       = bytes(0)
      val sender                        = PublicKeyAccount(bytes.slice(1, TransactionParsers.KeyLength + 1))
      val (assetName, descriptionStart) = Deser.parseArraySize(bytes, TransactionParsers.KeyLength + 1)
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
             proofs: Proofs): Either[ValidationError, TransactionT] =
    for {
      _ <- Either.cond(supportedVersions.contains(version), (), UnsupportedVersion(version))
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
                 timestamp: Long): Either[ValidationError, TransactionT] =
    for {
      unverified <- create(version, chainId, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, Proofs.empty)
      proofs     <- Proofs.create(Seq(ByteStr(crypto.sign(sender, unverified.bodyBytes()))))
    } yield unverified.copy(proofs = proofs)
}
