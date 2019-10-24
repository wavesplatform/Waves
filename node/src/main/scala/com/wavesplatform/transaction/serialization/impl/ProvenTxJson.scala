package com.wavesplatform.transaction.serialization.impl

import com.wavesplatform.common.utils.Base58
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.ProvenTransaction
import com.wavesplatform.transaction.serialization.{TxJson, TxBytes}
import play.api.libs.json.{JsArray, JsObject, JsString, Json}

object ProvenTxJson extends TxJson[ProvenTransaction] {
  override def toJson(tx: ProvenTransaction): JsObject = {
    import tx._
    Json.obj(
      "type"            -> typeId,
      "id"              -> id().toString,
      "sender"          -> sender.stringRepr,
      "senderPublicKey" -> Base58.encode(sender),
      "fee"             -> assetFee._2,
      "feeAssetId"      -> assetFee._1.maybeBase58Repr,
      "timestamp"       -> timestamp,
      "proofs"          -> JsArray(proofs.proofs.map(p => JsString(p.toString)))
    )
  }
}
