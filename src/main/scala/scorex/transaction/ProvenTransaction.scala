package scorex.transaction

import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash

trait ProvenTransaction extends Transaction with Proven {

  val version: Byte
  val bodyBytes: Coeval[Array[Byte]]

  val id: Coeval[AssetId] = Coeval.evalOnce(ByteStr(FastCryptographicHash(bodyBytes())))

  protected def jsonBase(): JsObject = Json.obj("type" -> transactionType.id,
    "version" -> version,
    "id" -> id().base58,
    "sender" -> sender.address,
    "senderPublicKey" -> Base58.encode(sender.publicKey),
    "fee" -> assetFee._2,
    "timestamp" -> timestamp,
    "proof" -> this.proof.base58
  )
}
