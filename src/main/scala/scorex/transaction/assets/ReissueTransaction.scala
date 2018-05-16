package scorex.transaction.assets

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.PublicKeyAccount
import scorex.crypto.signatures.Curve25519.KeyLength
import scorex.transaction.validation._
import scorex.transaction.{AssetId, ProvenTransaction, ValidationError, _}

trait ReissueTransaction extends ProvenTransaction {
  def assetId: ByteStr
  def quantity: Long
  def reissuable: Boolean
  def fee: Long
  def version: Byte
  def chainByte: Option[Byte]

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override final val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"    -> version,
      "chainId"    -> chainByte,
      "assetId"    -> assetId.base58,
      "quantity"   -> quantity,
      "reissuable" -> reissuable
    ))

  protected val bytesBase: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      sender.publicKey,
      assetId.arr,
      Longs.toByteArray(quantity),
      if (reissuable) Array(1: Byte) else Array(0: Byte),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )
  }
}

object ReissueTransaction {
  def validateReissueParams(quantity: Long, fee: Long): Either[ValidationError, Unit] =
    (validateAmount(quantity, "assets"), validateFee(fee))
      .mapN { case _ => () }
      .leftMap(_.head)
      .toEither

  def parseBase(bytes: Array[Byte], start: Int): (PublicKeyAccount, AssetId, Long, Boolean, Long, Long, Int) = {
    val senderEnd  = start + KeyLength
    val assetIdEnd = senderEnd + AssetIdLength
    val sender     = PublicKeyAccount(bytes.slice(start, senderEnd))
    val assetId    = ByteStr(bytes.slice(senderEnd, assetIdEnd))
    val quantity   = Longs.fromByteArray(bytes.slice(assetIdEnd, assetIdEnd + 8))
    val reissuable = bytes.slice(assetIdEnd + 8, assetIdEnd + 9).head == (1: Byte)
    val fee        = Longs.fromByteArray(bytes.slice(assetIdEnd + 9, assetIdEnd + 17))
    val end        = assetIdEnd + 25
    val timestamp  = Longs.fromByteArray(bytes.slice(assetIdEnd + 17, end))

    (sender, assetId, quantity, reissuable, fee, timestamp, end)
  }
}
