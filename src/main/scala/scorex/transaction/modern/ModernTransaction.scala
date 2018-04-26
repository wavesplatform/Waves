package scorex.transaction.modern

import com.google.common.primitives.{Bytes, Longs}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.PublicKeyAccount
import scorex.transaction._
import cats.implicits._
import scorex.crypto.encode.Base58
import scorex.transaction.base.TxBase

abstract class ModernTransaction(val builder: TransactionParser) extends FastHashId with TxBase {

  def header: TxHeader
  def payload: TxData
  def proofs: Proofs

  override def fee: Long = header.fee

  val headerBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      Array(0: Byte, header.`type`, header.version),
      header.sender.publicKey,
      Longs.toByteArray(header.timestamp),
      Longs.toByteArray(header.fee)
    )
  }

  override val bodyBytes: Coeval[Array[Byte]] =
    (headerBytes, payload.bytes)
      .mapN { case (h, p) => Bytes.concat(h, p) }

  override val bytes: Coeval[Array[Byte]] = //Coeval.evalOnce(Bytes.concat(headerBytes(), bodyBytes(), proofs.bytes()))
    (headerBytes, payload.bytes, proofs.bytes)
      .mapN { case (h, p, pr) => Bytes.concat(h, p, pr) }

  override val sender: PublicKeyAccount =
    header.sender

  override def timestamp: Long =
    header.timestamp

  val headerJson: Coeval[JsObject] = Coeval.evalOnce {
    Json.obj(
      "type"            -> builder.typeId,
      "version"         -> header.version,
      "id"              -> id().base58,
      "sender"          -> sender.address,
      "senderPublicKey" -> Base58.encode(sender.publicKey),
      "fee"             -> assetFee._2,
      "timestamp"       -> timestamp
    )
  }

  val proofsJson: Coeval[JsObject] =
    Coeval.evalOnce(Json.obj(proofField))

  override val json: Coeval[JsObject] =
    (headerJson, payload.json, proofsJson)
      .mapN {
        case (h, p, pr) => h ++ p ++ pr
      }
}
