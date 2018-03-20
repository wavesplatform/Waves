package scorex.transaction.assets

import com.google.common.base.Charsets
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import scorex.serialization.Deser
import scorex.transaction.TransactionParser._
import scorex.transaction.{ValidationError, _}
import scorex.transaction.smart.Script
import scorex.transaction.ValidationError.GenericError

import scala.util.{Failure, Success, Try}

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
      val sender                        = PublicKeyAccount(bytes.slice(2, KeyLength + 2))
      val (assetName, descriptionStart) = Deser.parseArraySize(bytes, KeyLength + 2)
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
      _ <- Either.cond(version == 1, (), GenericError(s"Unsupported SmartIssueTransaction version ${version.toInt}"))
      _ <- Either.cond(chainId == networkByte, (), GenericError(s"Wrong chainId ${chainId.toInt}"))
      _ <- IssueTransaction.create(sender, name, description, quantity, decimals, reissuable, fee, timestamp, ByteStr.empty)
    } yield SmartIssueTransaction(version, chainId, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, proofs)

  def create(version: Byte,
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
      _ <- Either.cond(version == 1, (), GenericError(s"Unsupported SmartIssueTransaction version ${version.toInt}"))
      _ <- Either.cond(chainId == networkByte, (), GenericError(s"Wrong chainId ${chainId.toInt}"))
      _ <- IssueTransaction.create(sender, name, description, quantity, decimals, reissuable, fee, timestamp, ByteStr.empty)
      unverified = SmartIssueTransaction(version, chainId, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, Proofs.empty)
      proofs <- Proofs.create(Seq(ByteStr(crypto.sign(sender, unverified.bodyBytes()))))

    } yield unverified.copy(proofs = proofs)
}

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

  override val assetFee: (Option[AssetId], Long)      = (None, fee)
  override val transactionType: TransactionType.Value = TransactionType.IssueTransaction

  val assetId = id

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      Array(transactionType.id.toByte),
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

  override val bytes = Coeval.evalOnce(Bytes.concat(Array(transactionType.id.toByte), signature.arr, bodyBytes()))

}

object IssueTransaction {
  val MaxDescriptionLength = 1000
  val MaxAssetNameLength   = 16
  val MinAssetNameLength   = 4
  val MaxDecimals          = 8

  def parseBytes(bytes: Array[Byte]): Try[IssueTransaction] = Try {
    require(bytes.head == TransactionType.IssueTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[IssueTransaction] =
    Try {
      val signature = ByteStr(bytes.slice(0, SignatureLength))
      val txId      = bytes(SignatureLength)
      require(txId == TransactionType.IssueTransaction.id.toByte, s"Signed tx id is not match")
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
             signature: ByteStr): Either[ValidationError, IssueTransaction] =
    for {
      _ <- Either.cond(quantity > 0, (), ValidationError.NegativeAmount(quantity, "assets"))
      _ <- Either.cond(description.length <= MaxDescriptionLength, (), ValidationError.TooBigArray)
      _ <- Either.cond(name.length >= MinAssetNameLength && name.length <= MaxAssetNameLength, (), ValidationError.InvalidName)
      _ <- Either.cond(decimals >= 0 && decimals <= MaxDecimals, (), ValidationError.TooBigArray)
      _ <- Either.cond(fee > 0, (), ValidationError.InsufficientFee)
    } yield IssueTransaction(sender, name, description, quantity, decimals, reissuable, fee, timestamp, signature)

  def create(sender: PrivateKeyAccount,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long): Either[ValidationError, IssueTransaction] =
    create(sender, name, description, quantity, decimals, reissuable, fee, timestamp, ByteStr.empty).right.map { unverified =>
      unverified.copy(signature = ByteStr(crypto.sign(sender, unverified.bodyBytes())))
    }
}
