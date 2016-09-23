package scorex.transaction.assets

import com.google.common.primitives.Longs
import play.api.libs.json.JsObject
import scorex.account.PublicKeyAccount
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.hash.FastCryptographicHash
import scorex.serialization.Deser
import scorex.transaction.{BalanceChange, Transaction}

import scala.util.Try

case class Issue(sender: PublicKeyAccount,
                 assetIdOpt: Option[Array[Byte]],
                 name: Array[Byte],
                 description: Array[Byte],
                 quantity: Long,
                 decimals: Byte,
                 reissuable: Boolean,
                 fee: Long,
                 timestamp: Long,
                 signature: Array[Byte]) extends Transaction {

  import Issue._

  require(description.length <= MaxDescriptionLength)
  require(name.length <= MaxAssetNameLength && name.length >= MinAssetNameLength)
  require(decimals >= 0 && decimals <= MaxDecimals)
  require(fee >= MinFee)
  require(quantity > 0)


  lazy val assetId = assetIdOpt.getOrElse(id)

  lazy val toSign: Array[Byte] = sender.publicKey ++ assetIdOpt.map(a => (1: Byte) +: a).getOrElse(Array(0: Byte)) ++
    arrayWithSize(name) ++ arrayWithSize(description) ++ Longs.toByteArray(quantity) ++ Array(decimals) ++
    (if (reissuable) Array(1: Byte) else Array(0: Byte)) ++ Longs.toByteArray(fee) ++ Longs.toByteArray(timestamp)

  override val id: Array[Byte] = FastCryptographicHash(toSign)

  override def json: JsObject = ???

  override def balanceChanges(): Seq[BalanceChange] = ???

  override def bytes: Array[Byte] = signature ++ toSign
}

object Issue extends Deser[Issue] {
  val MaxDescriptionLength = 1000
  val MaxAssetNameLength = 16
  val MinAssetNameLength = 4
  val MinFee = 100000000
  val MaxDecimals = 8

  override def parseBytes(bytes: Array[Byte]): Try[Issue] = Try {
    import EllipticCurveImpl._
    val signature = bytes.slice(0, SignatureLength)
    val sender = new PublicKeyAccount(bytes.slice(SignatureLength, SignatureLength + KeyLength))
    val assetIsDefined = bytes.slice(SignatureLength + KeyLength, SignatureLength + KeyLength + 1).head == (1: Byte)
    val assetIdOpt = if (!assetIsDefined) None
    else Some(bytes.slice(SignatureLength + KeyLength + 1, 2 * SignatureLength + KeyLength + 1))
    val nameStart = if (!assetIsDefined) SignatureLength + KeyLength + 1 else 2 * SignatureLength + KeyLength + 1
    val (assetName, descriptionStart) = parseArraySize(bytes, nameStart)
    val (description, quantityStart) = parseArraySize(bytes, descriptionStart)
    val quantity = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val decimals = bytes.slice(quantityStart + 8, quantityStart + 9).head
    val reissuable = bytes.slice(quantityStart + 9, quantityStart + 10).head == (1: Byte)
    val fee = Longs.fromByteArray(bytes.slice(quantityStart + 10, quantityStart + 18))
    val timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 18, quantityStart + 26))
    Issue(sender, assetIdOpt, assetName, description, quantity, decimals, reissuable, fee, timestamp, signature)
  }
}