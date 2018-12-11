package com.wavesplatform.transaction

import play.api.libs.json._
import com.wavesplatform.utils.Base58

trait ProvenTransaction extends Transaction with Proven {

  protected def proofField: Seq[(String, JsValue)] = Seq("proofs" -> JsArray(this.proofs.proofs.map(p => JsString(p.base58))))

  protected def jsonBase(): JsObject =
    Json.obj(
      "type"            -> builder.typeId,
      "id"              -> id().base58,
      "sender"          -> sender.address,
      "senderPublicKey" -> Base58.encode(sender.publicKey),
      "fee"             -> assetFee._2,
      "timestamp"       -> timestamp
    ) ++ JsObject(proofField)
}
