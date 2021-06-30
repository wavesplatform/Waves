package com.wavesplatform.transaction

import play.api.libs.json._

trait ProvenTransaction extends Proven { this: Transaction =>

  // TODO: Delete (use ProvenTXJson)
  protected def proofField: Seq[(String, JsValue)] = Seq("proofs" -> JsArray(this.proofs.proofs.map(p => JsString(p.toString))))

  protected def jsonBase(): JsObject =
    Json.obj(
      "type"            -> tpe.id,
      "id"              -> id().toString,
      "sender"          -> sender.toAddress.toString,
      "senderPublicKey" -> sender,
      "fee"             -> assetFee._2,
      "feeAssetId"      -> assetFee._1.maybeBase58Repr,
      "timestamp"       -> timestamp
    ) ++ JsObject(proofField)
}
