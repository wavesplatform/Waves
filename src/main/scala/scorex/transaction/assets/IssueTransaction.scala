package scorex.transaction.assets

import java.nio.charset.StandardCharsets

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import monix.eval.Coeval
import play.api.libs.json.Json
import scorex.account.PublicKeyAccount
import scorex.crypto.signatures.Curve25519
import scorex.serialization.Deser
import scorex.transaction.smart.script.Script
import scorex.transaction.validation._
import scorex.transaction.{AssetId, ProvenTransaction, ValidationError}

trait IssueTransaction extends ProvenTransaction {
  def name: Array[Byte]
  def description: Array[Byte]
  def quantity: Long
  def decimals: Byte
  def reissuable: Boolean
  def fee: Long
  def script: Option[Script]
  def version: Byte

  final lazy val assetId                               = id
  override final val assetFee: (Option[AssetId], Long) = (None, fee)

  override val json = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"     -> version,
      "assetId"     -> assetId().base58,
      "name"        -> new String(name, StandardCharsets.UTF_8),
      "quantity"    -> quantity,
      "reissuable"  -> reissuable,
      "decimals"    -> decimals,
      "description" -> new String(description, StandardCharsets.UTF_8),
      "script"      -> script.map(_.text)
    ))

  final protected val bytesBase: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      sender.publicKey,
      Deser.serializeArray(name),
      Deser.serializeArray(description),
      Longs.toByteArray(quantity),
      Array(decimals),
      Deser.serializeBoolean(reissuable),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    ))
}
object IssueTransaction {

  val MaxDescriptionLength = 1000
  val MaxAssetNameLength   = 16
  val MinAssetNameLength   = 4
  val MaxDecimals          = 8

  def validateIssueParams(name: Array[Byte],
                          description: Array[Byte],
                          quantity: Long,
                          decimals: Byte,
                          reissuable: Boolean,
                          fee: Long): Either[ValidationError, Unit] = {
    (
      validateAmount(quantity, "assets"),
      validateName(name),
      validateDescription(description),
      validateDecimals(decimals),
      validateFee(fee)
    ).mapN { case _ => () }
      .leftMap(_.head)
      .toEither
  }

  def parseBase(bytes: Array[Byte], start: Int) = {
    val sender                        = PublicKeyAccount(bytes.slice(start, start + Curve25519.KeyLength))
    val (assetName, descriptionStart) = Deser.parseArraySize(bytes, start + Curve25519.KeyLength)
    val (description, quantityStart)  = Deser.parseArraySize(bytes, descriptionStart)
    val quantity                      = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val decimals                      = bytes.slice(quantityStart + 8, quantityStart + 9).head
    val reissuable                    = bytes.slice(quantityStart + 9, quantityStart + 10).head == (1: Byte)
    val fee                           = Longs.fromByteArray(bytes.slice(quantityStart + 10, quantityStart + 18))
    val timestamp                     = Longs.fromByteArray(bytes.slice(quantityStart + 18, quantityStart + 26))
    (sender, assetName, description, quantity, decimals, reissuable, fee, timestamp, quantityStart + 26)
  }
}
