package com.wavesplatform.transaction.serialization.impl

import com.wavesplatform.transaction.smart.ContinuationTransaction
import play.api.libs.json.{JsObject, Json}

object ContinuationTxSerializer {
  def toJson(tx: ContinuationTransaction): JsObject = {
    import tx._
    Json.obj(
      "type"                      -> builder.typeId,
      "id"                        -> id().toString,
      "fee"                       -> fee,
      "timestamp"                 -> timestamp,
      "nonce"                     -> nonce,
      "invokeScriptTransactionId" -> invokeScriptTransactionId.toString
    )
  }
}
