package scorex.transaction

import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.encode.Base58

trait ProvenTransaction extends Transaction with Proven {

  protected def proofFieldName = "proofs"

  val bodyBytes: Coeval[Array[Byte]]

  protected def jsonBase(): JsObject = Json.obj("type" -> transactionType.id,
    "id" -> id().base58,
    "sender" -> sender.address,
    "senderPublicKey" -> Base58.encode(sender.publicKey),
    "fee" -> assetFee._2,
    "timestamp" -> timestamp,
     proofFieldName -> this.proofs.map(_.base58)
  )
}
