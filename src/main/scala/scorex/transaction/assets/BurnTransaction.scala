package scorex.transaction.assets

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.PublicKeyAccount
import scorex.crypto.signatures.Curve25519.KeyLength
import scorex.transaction._
import scorex.transaction.validation._

trait BurnTransaction extends ProvenTransaction {
  def version: Byte

  def chainByte: Option[Byte]

  def assetId: ByteStr

  def amount: Long

  def fee: Long

  def timestamp: Long

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "chainId" -> chainByte,
      "version" -> version,
      "assetId" -> assetId.base58,
      "amount"  -> amount,
      "fee"     -> fee
    )
  }

  val byteBase: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      sender.publicKey,
      assetId.arr,
      Longs.toByteArray(amount),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )
  }
}

object BurnTransaction {

  val typeId: Byte = 6

  def parseBase(start: Int, bytes: Array[Byte]): (PublicKeyAccount, AssetId, Long, Long, Long, Int) = {
    val sender        = PublicKeyAccount(bytes.slice(start, start + KeyLength))
    val assetId       = ByteStr(bytes.slice(start + KeyLength, start + KeyLength + AssetIdLength))
    val quantityStart = start + KeyLength + AssetIdLength

    val quantity  = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val fee       = Longs.fromByteArray(bytes.slice(quantityStart + 8, quantityStart + 16))
    val timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 16, quantityStart + 24))

    (sender, assetId, quantity, fee, timestamp, quantityStart + 24)
  }

  def validateBurnParams(amount: Long, fee: Long): Either[ValidationError, Unit] = {
    (validateAmount(amount, "assets"), validateFee(fee))
      .mapN { case _ => () }
      .fold(
        _.head.asLeft[Unit],
        _.asRight[ValidationError]
      )
  }
}
